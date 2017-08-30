module Wad ( getWadDirectory
           , parseDirectory
           , DirEnt
           , WadHeader
           , headerSize
           ) where

import qualified Data.ByteString.Lazy as L
import Data.Binary.Get
import Data.Word
import Data.Int (Int32)

type WadHeader = (L.ByteString, Int32, Int32)
type DirEnt = (Int32, Int32, L.ByteString)
headerSize = 12 -- 4 magic, 4 numents, 4 diroffs
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
