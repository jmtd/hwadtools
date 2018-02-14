{-
    wadcat.hs: concatenate a series of WADs into one
    Part of HWadTools
    Copyright © Jonathan Dowland 2018

    Distributed under the terms of the GNU GPL Version 3
    See file LICENSE
 -}

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as C
import Data.Binary (encode, decode)
import Data.Binary.Get
import System.IO (stdin, IOMode(..), hSetBinaryMode, openFile, Handle, hClose)
import System.Environment (getArgs)
import Data.Int (Int32)

import Wad

getHandles :: IO ([Handle], Handle)
getHandles = do
    args    <- getArgs
    out     <- openFile (head $ reverse args) WriteMode
    handles <- mapM (\x-> openFile x ReadMode) $ (reverse.tail.reverse) args

    hSetBinaryMode out True
    mapM_ (\x->hSetBinaryMode x True) handles

    return (handles, out)

main = do
    (handles, out) <- getHandles
    inputs         <- mapM L.hGetContents handles -- :: [L.ByteString]

    let dirs       = map wadDirEnts inputs -- :: [[DirEnt]]
        headers    = map decode inputs :: [WadHeader]
        outNuments = fromIntegral $ sum $ map length dirs
        outDiroffs = wadHeaderSize + (fromIntegral $ sum $ map (\wh->((diroffs wh) - (fromIntegral wadHeaderSize))) headers)
        outHeader  = WadHeader (LC.pack "PWAD") outNuments (fromIntegral outDiroffs)
        outDirs    = offsetDirEnts 0 (zip headers dirs)
        in do
            L.hPut out (encode outHeader)
            mapM_ (writeLumps out) $ zip headers inputs
            mapM_ (L.hPut out) $ concat $ map (map encode) outDirs
            hClose out

writeLumps :: Handle -> (WadHeader, L.ByteString) -> IO ()
writeLumps out (header,input) = do
    L.hPut out $ drop wadHeaderSize $ take (diroffs header) input
    where
        take x = L.take (fromIntegral x)
        drop x = L.drop (fromIntegral x)

offsetDirEnts :: Int32 -> [(WadHeader, [DirEnt])] -> [[DirEnt]]
offsetDirEnts _ [] = []
offsetDirEnts acc (((WadHeader _ _ offs),de):tail) =
    (map (offsetDirEnt acc) de):(offsetDirEnts (acc+offs-whSize) tail)
    where whSize = fromIntegral wadHeaderSize

offsetDirEnt :: Int32 -> DirEnt -> DirEnt
offsetDirEnt n (DirEnt fp size name) = DirEnt (n+fp) size name
