{-# OPTIONS_GHC -O2 #-}

import Data.Aeson.Parser.UnescapeFFI as FFI
import Data.Aeson.Parser.UnescapePure as Pure
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import System.Environment

main = do
    [mode,n] <- getArgs
    let f = case mode of
            "ffi" -> FFI.unescapeText
            "pure" -> Pure.unescapeText
            _ -> error "Unknown mode"
    putStrLn $ either show (show . T.length) $ f $ BS.concat $ replicate (read n) $ BS.pack "\\\""

