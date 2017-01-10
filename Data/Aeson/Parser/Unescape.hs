{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE UnliftedFFITypes         #-}
{-# LANGUAGE Strict                   #-}

module Data.Aeson.Parser.Unescape (
  unescapeText
) where

import           Control.Exception          (evaluate, throw, try)
import           Control.Monad              (when)
import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString as A
import           Data.ByteString            as B
import           Data.Bits                  (shiftL, shiftR, (.&.), (.|.))
import           Data.Monoid                (mempty, (<>))
import           Data.Text                  (Text)
import qualified Data.Text.Array            as A
import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.Builder     as T
import           Data.Text.Encoding.Error   (UnicodeException (..))
import           Data.Text.Internal.Private (runText)
import           Data.Text.Unsafe           (unsafeDupablePerformIO)
import           Data.Word                  (Word8, Word16, Word32)

-- Different UTF states.
data Utf = 
    UtfGround
  | UtfTail1
  | UtfU32e0
  | UtfTail2
  | UtfU32ed
  | Utf843f0
  | UtfTail3
  | Utf843f4
  deriving (Eq)

data State = 
    StateNone
  | StateUtf !Utf !Word32
  | StateBackslash
  | StateU0
  | StateU1 !Word16
  | StateU2 !Word16
  | StateU3 !Word16
  | StateS0
  | StateS1
  | StateSU0
  | StateSU1 !Word16
  | StateSU2 !Word16
  | StateSU3 !Word16
  deriving (Eq)

-- Referenced: https://github.com/jwilm/vte/blob/master/utf8parse/src/table.rs.in
setByte1 point word = point .|. fromIntegral (word .&. 0x3f)
{-# INLINE setByte1 #-}
setByte2 point word = point .|. ((fromIntegral $ word .&. 0x3f) `shiftL` 6)
{-# INLINE setByte2 #-}
setByte2Top point word = point .|. ((fromIntegral $ word .&. 0x1f) `shiftL` 6)
{-# INLINE setByte2Top #-}
setByte3 point word = point .|. ((fromIntegral $ word .&. 0x3f) `shiftL` 12)
{-# INLINE setByte3 #-}
setByte3Top point word = point .|. ((fromIntegral $ word .&. 0xf) `shiftL` 12)
{-# INLINE setByte3Top #-}
setByte4 point word = point .|. ((fromIntegral $ word .&. 0x7) `shiftL` 18)
{-# INLINE setByte4 #-}

decode :: Utf -> Word32 -> Word8 -> Parser (Utf, Word32)
decode UtfGround point word = case word of
  w | 0x00 <= w && w <= 0x7f -> return (UtfGround, fromIntegral word)
  w | 0xc2 <= w && w <= 0xdf -> return (UtfTail1, setByte2Top point word)
  0xe0                       -> return (UtfU32e0, setByte3Top point word)
  w | 0xe1 <= w && w <= 0xec -> return (UtfTail2, setByte3Top point word)
  0xed                       -> return (UtfU32ed, setByte3Top point word)
  w | 0xee <= w && w <= 0xef -> return (UtfTail2, setByte3Top point word)
  0xf0                       -> return (Utf843f0, setByte4 point word)
  w | 0xf1 <= w && w <= 0xf3 -> return (UtfTail3, setByte4 point word)
  0xf4                       -> return (Utf843f4, setByte4 point word)
  _                          -> throwDecodeError

decode UtfU32e0 point word = case word of
  w | 0xa0 <= w && w <= 0xbf -> return (UtfTail1, setByte2 point word)
  _                          -> throwDecodeError

decode UtfU32ed point word = case word of 
  w | 0x80 <= w && w <= 0x9f -> return (UtfTail1, setByte2 point word)
  _                          -> throwDecodeError

decode Utf843f0 point word = case word of
  w | 0x90 <= w && w <= 0xbf -> return (UtfTail2, setByte3 point word)
  _                          -> throwDecodeError

decode Utf843f4 point word = case word of
  w | 0x80 <= w && w <= 0x8f -> return (UtfTail2, setByte3 point word)
  _                          -> throwDecodeError

decode UtfTail3 point word = case word of
  w | 0x80 <= w && w <= 0xbf -> return (UtfTail2, setByte3 point word)
  _                          -> throwDecodeError

decode UtfTail2 point word = case word of
  w | 0x80 <= w && w <= 0xbf -> return (UtfTail1, setByte2 point word)
  _                          -> throwDecodeError

decode UtfTail1 point word = case word of
  w | 0x80 <= w && w <= 0xbf -> return (UtfGround, setByte1 point word)
  _                          -> throwDecodeError

{-# INLINE decode #-}

decodeHex :: Word8 -> Parser Word16
decodeHex 48  = return 0  -- '0' 
decodeHex 49  = return 1  -- '1' 
decodeHex 50  = return 2  -- '2' 
decodeHex 51  = return 3  -- '3' 
decodeHex 52  = return 4  -- '4' 
decodeHex 53  = return 5  -- '5' 
decodeHex 54  = return 6  -- '6' 
decodeHex 55  = return 7  -- '7' 
decodeHex 56  = return 8  -- '8' 
decodeHex 57  = return 9  -- '9' 
decodeHex 65  = return 10 -- 'A' 
decodeHex 97  = return 10 -- 'a' 
decodeHex 66  = return 11 -- 'B' 
decodeHex 98  = return 11 -- 'b' 
decodeHex 67  = return 12 -- 'C' 
decodeHex 99  = return 12 -- 'c' 
decodeHex 68  = return 13 -- 'D' 
decodeHex 100 = return 13 -- 'd' 
decodeHex 69  = return 14 -- 'E' 
decodeHex 101 = return 14 -- 'e' 
decodeHex 70  = return 15 -- 'F' 
decodeHex 102 = return 15 -- 'f' 
decodeHex _ = throwDecodeError
{-# INLINE decodeHex #-}

unescapeText :: Parser Text
unescapeText = do
    -- (dest, pos, finalState)
    loop mempty StateNone

    -- Check final state.
    -- when ( finalState /= StateNone)
    --   throwDecodeError

    -- return $ runText $ \done -> do
    --   done dest pos

    where
      len = 10000 -- TODO: B.length bs XXX

      loop m state = do
        c <- A.anyWord8
        resM <- f m state c
        case resM of
          Nothing ->
            -- TODO: Check final state. Not necessary here? XXX
            -- error "TODO"

            return $ T.toStrict $ T.toLazyText m

          Just (m', state') -> 
            loop m' state'

      runUtf m st point c = do
        r <-  decode st point c
        case r of
          (UtfGround, 92) -> -- \
              return $ Just (m, StateBackslash)
          (UtfGround, w) | w <= 0xffff -> 
              writeAndReturn m (fromIntegral w) StateNone
          (UtfGround, w) -> 
              writeAndReturn 
                (write m (0xd7c0 + fromIntegral (w `shiftR` 10)))
                (0xdc00 + fromIntegral (w .&. 0x3ff))
                StateNone
          (st, p) -> 
              return $ Just (m, StateUtf st p)

      {-# INLINE runUtf #-}

      -- f' dest m c = m >>= \s -> f dest s c

      -- {-# INLINE f' #-}

      -- Hit closing quote.
      f m StateNone 34 = return Nothing

      -- No pending state.
      f m StateNone c = runUtf m UtfGround 0 c

      -- In the middle of parsing a UTF string.
      f m (StateUtf st point) c = runUtf m st point c

      -- In the middle of escaping a backslash.
      f m StateBackslash  34 = writeAndReturn m 34 StateNone -- "
      f m StateBackslash  92 = writeAndReturn m 92 StateNone -- \
      f m StateBackslash  47 = writeAndReturn m 47 StateNone -- /
      f m StateBackslash  98 = writeAndReturn m  8 StateNone -- b
      f m StateBackslash 102 = writeAndReturn m 12 StateNone -- f
      f m StateBackslash 110 = writeAndReturn m 10 StateNone -- n
      f m StateBackslash 114 = writeAndReturn m 13 StateNone -- r
      f m StateBackslash 116 = writeAndReturn m  9 StateNone -- t
      f m StateBackslash 117 = return $ Just (m, StateU0)           -- u
      f m StateBackslash _   = throwDecodeError

      -- Processing '\u'.
      f m StateU0 c = do
        w <- decodeHex c
        return $ Just (m, StateU1 (w `shiftL` 12))

      f m (StateU1 w') c = do
        w <- decodeHex c
        return $ Just (m, StateU2 (w' .|. (w `shiftL` 8)))

      f m (StateU2 w') c = do
        w <- decodeHex c
        return $ Just (m, StateU3 (w' .|. (w `shiftL` 4)))

      f m (StateU3 w') c = do
        w <- decodeHex c
        let u = w' .|. w

        -- Get next state based on surrogates.
        st <- 
              if u >= 0xd800 && u <= 0xdbff then -- High surrogate.
                return StateS0
              else if u >= 0xdc00 && u <= 0xdfff then -- Low surrogate.
                throwDecodeError
              else
                return StateNone
        writeAndReturn m u st

      -- Handle surrogates.
      f m StateS0 92 = return $ Just (m, StateS1) -- \
      f _ StateS0  _ = throwDecodeError

      f m StateS1 117 = return $ Just (m, StateSU0) -- u
      f _ StateS1   _ = throwDecodeError

      f m StateSU0 c = do
        w <- decodeHex c
        return $ Just (m, StateSU1 (w `shiftL` 12))

      f m (StateSU1 w') c = do
        w <- decodeHex c
        return $ Just (m, StateSU2 (w' .|. (w `shiftL` 8)))

      f m (StateSU2 w') c = do
        w <- decodeHex c
        return $ Just (m, StateSU3 (w' .|. (w `shiftL` 4)))

      f m (StateSU3 w') c = do
        w <- decodeHex c
        let u = w' .|. w

        -- Check if not low surrogate.
        if u < 0xdc00 || u > 0xdfff then
          throwDecodeError
        else
          writeAndReturn m u StateNone

      {-# INLINE f #-}

{-# INLINE unescapeText #-}

-- TODO: We should be able to improve this... XXX
fromWord16 char = T.fromText $ runText $ \done -> do
  dest <- A.new 1
  A.unsafeWrite dest 0 char
  done dest 1

write m char = do
  m <> fromWord16 char
{-# INLINE write #-}

writeAndReturn m char res = 
  return $ Just (write m char, res)
{-# INLINE writeAndReturn #-}
      
throwDecodeError = 
  fail "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream"

-- throwDecodeError :: a
-- throwDecodeError = 
--   let desc = "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream" in
--   throw (DecodeError desc Nothing)
{-# INLINE throwDecodeError #-}




{-
import           Control.Exception          (evaluate, throw, try)
import           Control.Monad.ST.Unsafe    (unsafeIOToST, unsafeSTToIO)
import           Data.ByteString            as B
import           Data.ByteString.Internal   as B hiding (c2w)
import qualified Data.Text.Array            as A
import           Data.Text.Encoding.Error   (UnicodeException (..))
import           Data.Text.Internal         (Text (..))
import           Data.Text.Internal.Private (runText)
import           Data.Text.Unsafe           (unsafeDupablePerformIO)
import           Data.Word                  (Word8)
import           Foreign.C.Types            (CInt (..), CSize (..))
import           Foreign.ForeignPtr         (withForeignPtr)
import           Foreign.Marshal.Utils      (with)
import           Foreign.Ptr                (Ptr, plusPtr)
import           Foreign.Storable           (peek)
import           GHC.Base                   (MutableByteArray#)

foreign import ccall unsafe "_js_decode_string" c_js_decode
    :: MutableByteArray# s -> Ptr CSize
    -> Ptr Word8 -> Ptr Word8 -> IO CInt

unescapeText' :: ByteString -> Text
unescapeText' (PS fp off len) = runText $ \done -> do
  let go dest = withForeignPtr fp $ \ptr ->
        with (0::CSize) $ \destOffPtr -> do
          let end = ptr `plusPtr` (off + len)
              loop curPtr = do
                res <- c_js_decode (A.maBA dest) destOffPtr curPtr end
                case res of
                  0 -> do
                    n <- peek destOffPtr
                    unsafeSTToIO (done dest (fromIntegral n))
                  _ ->
                    throw (DecodeError desc Nothing)
          loop (ptr `plusPtr` off)
  (unsafeIOToST . go) =<< A.new len
 where
  desc = "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream"
{-# INLINE unescapeText' #-}
-}

-- unescapeText :: ByteString -> Either UnicodeException Text
-- unescapeText = unsafeDupablePerformIO . try . evaluate . unescapeText'
-- {-# INLINE unescapeText #-}
