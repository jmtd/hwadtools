module Util(clean,pad,getHandle) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import System.IO (stdin, IOMode(..), hSetBinaryMode, openFile)
import System.Environment (getArgs)

clean :: B.ByteString -> String
clean bs = clean' bs 8 where
    clean' _ 0 = ""
    clean' bs n | bs == B.empty = replicate n ' '
                | head == '\0'  = replicate n ' '
                | otherwise     = head : (clean' tail (n-1))
        where
            head = C.head bs
            tail = C.tail bs

pad :: String -> String
pad s = s ++ (replicate (8 - (length s)) ' ')

getHandle = do
    args <- getArgs
    handle <- if args == []
              then return stdin
              else openFile (head args) ReadMode
    hSetBinaryMode handle True
    return handle
