{-
    waddiff.hs: print out the difference between two WADs
    Part of HWadTools
    Copyright © Jonathan Dowland 2018

    Distributed under the terms of the GNU GPL Version 3
    See file LICENSE
 -}

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Data.Binary.Get
import System.IO (stdin, IOMode(..), hSetBinaryMode, openFile, Handle)
import System.Environment (getArgs)
import Data.Algorithm.Diff
import Data.Algorithm.DiffContext
import Text.PrettyPrint

import Wad

clean :: B.ByteString -> String
clean bs = clean' bs 8 where
    clean' _ 0 = ""
    clean' bs n | bs == B.empty = replicate n ' '
                | head == '\0'  = replicate n ' '
                | otherwise     = head : (clean' tail (n-1))
        where
            head = C.head bs
            tail = C.tail bs

getHandles :: IO (Handle, Handle)
getHandles = do
    args <- getArgs
    handle1 <-  openFile (head args) ReadMode
    handle2 <-  openFile (head $ tail args) ReadMode
    hSetBinaryMode handle1 True
    hSetBinaryMode handle2 True
    return (handle1,handle2)

printDir :: DirEnt -> IO String
printDir (offs,size,rawname) = do
    return $ name ++ "\t" ++ (show size)
    where
        name = (clean . L.toStrict) rawname

getEntries :: Handle -> IO ([String])
getEntries h = do
    input <- L.hGetContents h
    let (numents,waddir) = runGet getWadDirectory input
    let dirents = runGet (parseDirectory numents) waddir
    mapM printDir dirents

textCompare :: [String] -> [String] -> String
textCompare as bs = render bar where
    bar = prettyContextDiff (char 'a') (char 'b') text baz -- :: Doc
    baz = getContextDiff 3 as bs

main = do
    (handle1, handle2) <- getHandles
    names1 <- getEntries handle1
    names2 <- getEntries handle2
    putStrLn $ textCompare names1 names2
