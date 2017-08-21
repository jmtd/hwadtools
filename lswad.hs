{-
    lswad.hs: list the contents of Doom WAD files
    Part of HWadTools
    Copyright © Jonathan Dowland 2017

    Distributed under the terms of the GNU GPL Version 3
    See file LICENSE
 -}

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Data.Binary.Get
import Data.Word
import Data.Int -- Int32
import System.IO -- stdin
import System.Environment -- getArgs

type WadHeader = (L.ByteString, Int32, Int32)

type DirEnt = (Int32, Int32, L.ByteString)
direntSize = 16 -- 4 filepos, 4 size, 8 name

deserialiseHeader :: Get WadHeader
deserialiseHeader = do
  magic   <- getLazyByteString 4
  numents <- getInt32le
  diroffs <- getInt32le
  return (magic, numents, diroffs)

getWadDirectory :: Get (Int32, L.ByteString)
getWadDirectory = do
  (_,numents,diroffs) <- deserialiseHeader
  skip $ fromIntegral (diroffs - 12)
  dir <- getLazyByteString $ fromIntegral (numents * direntSize)
  return (numents, dir)

parseDirEnt :: Get DirEnt
parseDirEnt = do
  offset <- getInt32le
  size   <- getInt32le
  name   <- getLazyByteString 8
  return (offset,size,name)

parseDirectory :: Int32 -> Get [DirEnt]
parseDirectory 0 = return []
parseDirectory n = do
    head <- parseDirEnt
    tail <- parseDirectory (n - 1)
    return (head : tail)

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
printDir (offs,size,rawname) = do
    putStrLn $ name ++ "\t" ++ ((pad.show) size) ++ "\t" ++ (show offs)
    where
        name = (clean . L.toStrict) rawname

header = "  name  \t  size  \t offset "

main = do
  handle <- getHandle
  input  <- L.hGetContents handle
  let (numents,waddir) = runGet getWadDirectory input
  let dirents = runGet (parseDirectory numents) waddir
  putStrLn header
  mapM_ printDir dirents
