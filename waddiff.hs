{-
    waddiff.hs: print out the difference between two WADs
    Part of HWadTools
    Copyright © Jonathan Dowland 2018

    Distributed under the terms of the GNU GPL Version 3
    See file LICENSE
 -}

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as C
import Data.Binary.Get
import System.IO (stdin, IOMode(..), hSetBinaryMode, openFile, Handle)
import System.Environment (getArgs)
import Data.Algorithm.Diff
import Data.Algorithm.DiffContext
import Text.PrettyPrint
import Data.Int (Int32)

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

data WadEntry = WadEntry String Integer L.ByteString deriving (Eq)

-- we don't try and show the whole lump
instance Show WadEntry where
    show (WadEntry n l d) = n ++ " (" ++ (show l) ++ " bytes)"

getEntries :: Handle -> IO ([WadEntry])
getEntries h = do
    input <- L.hGetContents h
    let dirents = wadDirEnts input
        mkEntry (DirEnt o s r) = WadEntry (clean.L.toStrict $ r) (fromIntegral s) (L.take (fromIntegral s) $ L.drop (fromIntegral o) input)
        in
        return $ map mkEntry dirents

textCompare x a b = render $ prettyContextDiff (text a) (text b) (text.show) x

main = do
    args <- getArgs
    (handle1, handle2) <- getHandles
    entries1 <- getEntries handle1
    entries2 <- getEntries handle2
    let diff = getContextDiff 3 entries1 entries2
        file1 = head args
        file2 = head $ tail args
        in
        putStrLn $ textCompare diff file1 file2

------------------------------------------------------------------------------
-- test data

test1 = [ WadEntry "HI" 2 $ LC.pack "hi"
        , WadEntry "There" 5 $ LC.pack "there"
        ]

test2 = (WadEntry "yo" 2 $ LC.pack "yo"):test1
