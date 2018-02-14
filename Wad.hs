module Wad ( WadHeader(..)
           , wadHeaderSize
           , DirEnt(..)
           , getDirEnts
           , wadDirEnts
           ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put (putInt32le, putLazyByteString)
import Data.Word
import Data.Int (Int16, Int32, Int64)

------------------------------------------------------------------------------
-- Wad Header

data WadHeader = WadHeader { magic   :: L.ByteString
                           , numents :: Int32
                           , diroffs :: Int32 } deriving (Show, Eq)
wadHeaderSize = 12

instance Binary (WadHeader) where
    get = do magic   <- getLazyByteString 4
             numents <- getInt32le
             diroffs <- getInt32le
             return (WadHeader magic numents diroffs)

    put header = do putLazyByteString (magic header)
                    putInt32le (numents header)
                    putInt32le (diroffs header)

sample_wh = WadHeader {magic = LC.pack "PWAD", numents = 7, diroffs = 295639}
sample_wh_packed = LC.pack "PWAD\a\NUL\NUL\NUL\215\130\EOT\NUL"

test_wadheader_binary_idem1 = sample_wh == (decode sample_wh_packed)
test_wadheader_binary_idem2 = sample_wh_packed == (encode sample_wh)
-- prop_wadheader_binary_idem1 x = x == decode (encode x)
-- prop_wadheader_binary_idem2 x = x == encode (decode x)

------------------------------------------------------------------------------
-- Wad Directory

data DirEnt = DirEnt Int32 Int32 L.ByteString deriving (Show)
direntSize = 16 -- 4 filepos, 4 size, 8 name

instance Binary DirEnt where
    get = do offset <- getInt32le
             size   <- getInt32le
             name   <- getLazyByteString 8
             return $ DirEnt offset size name

    put (DirEnt fp size name) = do putInt32le fp
                                   putInt32le size
                                   putLazyByteString name

-- XXX: test wad directory encoding/decoding
example_dir_packed = LC.pack "\f\0\0\0\0\0\0\0MAP01\0\0\0\f\0\0\0\n\0\0\0THINGS\
                             \\0\0\SYN\0\0\0N\187\0\0LINEDEFSd\187\0\0\CAN!\ETX\
                             \\0SIDEDEFS|\220\ETX\0X#\0\0VERTEXES\212\255\ETX\0\
                             \8v\0\0SECTORS\0\fv\EOT\0\203\f\0\0WADCSRC\0"

example_dirent_packed = LC.pack "\f\0\0\0\0\0\0\0MAP01\0\0\0"

--- XXX need a way to handle a list of dirents

getDirEnts :: Int32 -> Get [DirEnt]
getDirEnts 0 = return []
getDirEnts n = do
  head <- get
  tail <- getDirEnts (n - 1)
  return (head : tail)

-- it would be nicer if this operated on a bytestring pointing
-- at the directory offset already; and didn't require a numents
-- parameter in that case, but just kept going
wadDirEnts :: L.ByteString -> [DirEnt]
wadDirEnts input = let wh   = decode input :: WadHeader
                       nume = fromIntegral $ numents wh
                       offs = fromIntegral $ diroffs wh
                       dir  = L.drop offs input
                       in
                           runGet (getDirEnts nume) dir
