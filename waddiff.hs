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
import System.Environment (getArgs, withArgs)
import Data.Algorithm.Diff
import Data.Algorithm.DiffContext
import Text.PrettyPrint
import Data.Int (Int32)

import Wad
import Util

data WadEntry = WadEntry String Integer L.ByteString deriving (Eq)

-- we don't try and show the whole lump
instance Show WadEntry where
    show (WadEntry n l d) = n ++ " (" ++ (show l) ++ " bytes)"

mkEntry input (DirEnt o s r) = let
    s' = fromIntegral s
    s''= fromIntegral s
    o' = fromIntegral o
    in WadEntry (clean . L.toStrict $ r) s' (L.take s'' . L.drop o' $ input)

getEntries :: Handle -> IO ([WadEntry])
getEntries h = do
    input <- L.hGetContents h
    return $ map (mkEntry input) (wadDirEnts input)

textCompare a b = render . prettyContextDiff (text a) (text b) (text.show)

main = do
    args     <- getArgs
    handle1  <- getHandle
    handle2  <- withArgs (tail args) getHandle
    entries1 <- getEntries handle1
    entries2 <- getEntries handle2
    let diff  = getContextDiff 3 entries1 entries2
        file1 = args !! 0
        file2 = args !! 1
        in
        putStrLn $ textCompare file1 file2 diff

------------------------------------------------------------------------------
-- test data

test1 = [ WadEntry "HI" 2 $ LC.pack "hi"
        , WadEntry "There" 5 $ LC.pack "there"
        ]

test2 = (WadEntry "yo" 2 $ LC.pack "yo"):test1
