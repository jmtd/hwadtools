{-
    lswad.hs: list the contents of Doom WAD files
    Part of HWadTools
    Copyright © Jonathan Dowland 2018

    Distributed under the terms of the GNU GPL Version 3
    See file LICENSE
 -}

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Data.Binary (decode)
import Data.Binary.Get
import System.IO (stdin, IOMode(..), hSetBinaryMode, openFile)
import System.Environment (getArgs)

import Wad

-- test
s = C.pack $ "MAP01\0\0" ++ "1"

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

printDir :: DirEnt -> IO ()
printDir (DirEnt offs size rawname) = do
    putStrLn $ name ++ "\t" ++ ((pad.show) size) ++ "\t" ++ (show offs)
    where
        name = (clean . L.toStrict) rawname

main = do
    handle <- getHandle
    input  <- L.hGetContents handle
    putStrLn "  name  \t  size  \t offset "
    mapM_ printDir $ wadDirEnts input
