{-
    wadcat.hs: concatenate a series of WADs into one
    Part of HWadTools
    Copyright © Jonathan Dowland 2018

    Distributed under the terms of the GNU GPL Version 3
    See file LICENSE
 -}

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Binary (encode, decode)
import Data.Int (Int32)
import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.IO

import Wad

getHandles :: IO [Handle]
getHandles = do
    args    <- getArgs
    handles <- mapM (\x-> openFile x ReadMode) args

    mapM_ (\x->hSetBinaryMode x True) handles

    return handles

ensureOutputRedirected :: IO ()
ensureOutputRedirected = do
    redirected <- fmap not $ hIsTerminalDevice stdout
    if redirected then return () else do
        hPutStrLn stderr "Error: standard output is a terminal. You must redirect wadcat's output."
        exitFailure

main = do
    ensureOutputRedirected

    handles <- getHandles
    inputs  <- mapM L.hGetContents handles -- :: [L.ByteString]

    let dirs       = map wadDirEnts inputs -- :: [[DirEnt]]
        headers    = map decode inputs :: [WadHeader]
        outNuments = fromIntegral $ sum $ map length dirs
        outDiroffs = wadHeaderSize + (fromIntegral $ sum $ map (\wh->((diroffs wh) - (fromIntegral wadHeaderSize))) headers)
        outHeader  = WadHeader (LC.pack "PWAD") outNuments (fromIntegral outDiroffs)
        outDirs    = offsetDirEnts 0 (zip headers dirs)
        in do
            L.putStr         $ encode outHeader
            mapM_ writeLumps $ zip headers inputs
            mapM_ L.putStr   $ concat $ map (map encode) outDirs

writeLumps :: (WadHeader, L.ByteString) -> IO ()
writeLumps (header,input) = do
    L.putStr $ drop wadHeaderSize $ take (diroffs header) input
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
