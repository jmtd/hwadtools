{-
    wadstats.hs: output some statistics about MAPs in WAD files
    Part of HWadTools
    Copyright © Jonathan Dowland 2018

    Distributed under the terms of the GNU GPL Version 3
    See file LICENSE
 -}
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as C
import Data.Binary (decode)
import Data.Binary.Get
import System.IO (stdin, IOMode(..), hSetBinaryMode, openFile)
import System.Environment (getArgs)
import Data.Int (Int16, Int64)
import Data.List (partition)
import Data.List.Unique (count)
import Data.Char (toUpper)

import Wad
import Bits
import Util

import qualified Data.IntMap.Strict as M
import qualified Doom.Things as T

main = do
    handle <- getHandle
    input  <- L.hGetContents handle
    let dirents = wadDirEnts input
    mapStats dirents input

mapStats :: [DirEnt] -> L.ByteString -> IO ()
mapStats ds input = case nextMapLabel ds of
    []     -> return ()
    (x:xs) -> do
                  (putStrLn . ("- "++) . (++":") . prepare . LC.unpack . lumpName) x
                  mapStat  (x:xs) input
                  mapStats xs input

nextMapLabel :: [DirEnt] -> [DirEnt]
nextMapLabel (x:xs) = if isMap (x:xs) then (x:xs) else nextMapLabel xs
nextMapLabel _ = []

isMap :: [DirEnt] -> Bool
isMap (x:d@(DirEnt _ _ label):xs) = (prepare . LC.unpack) label == "THINGS"
isMap _ = False

-- capture the logic of C's strncasecmp, necessary for comparing lump names
-- for equality following precisely Doom's logic
prepare :: String -> String
prepare = map toUpper . takeWhile (/= '\0') . take 8

mapStat :: [DirEnt] -> L.ByteString -> IO ()
mapStat map input = do
    doIfFind "VERTEXES" map (blah 4 "vertices")
    doIfFind "SECTORS"  map (blah 26 "sectors")
    doIfFind "LINEDEFS" map (blah 14 "linedefs") -- doom format
    case (findSomething "THINGS" map) of
      Nothing -> return ()
      Just t  -> do
                     let count = ((lumpSize . head) t) `div` 10
                     putStrLn $ "    things:\n      count: " ++ (show count)
                     thingStats $ lump (head t) input
                     where
                         lump (DirEnt offs len _) input = take len $ drop offs input
                         take x = L.take (fromIntegral x)
                         drop x = L.drop (fromIntegral x)

doIfFind :: String -> [DirEnt] -> ([DirEnt] -> IO ()) -> IO ()
doIfFind needle haystack action = let vs = findSomething needle haystack in
    case vs of
    Nothing -> return ()
    Just v  -> action v

blah :: Int -> String -> [DirEnt] -> IO ()
blah i ty v = let count = ((fromIntegral . lumpSize . head) v) `div` i
           in  putStrLn $ "    " ++ ty ++ ": " ++ (show count)

findSomething :: String -> [DirEnt] -> Maybe [DirEnt]
findSomething l ds = let pred = not . (== (prepare l)) . prepare . LC.unpack . lumpName
                   in case dropWhile pred ds of
                     [] -> Nothing
                     d  -> Just d

thingStats :: L.ByteString -> IO ()
thingStats lump = do
    let
        numthings = (L.length lump) `div` 10
        things = runGet (readThings numthings) lump
        (mp,sp) = partition (multiplayer.thingflags) things
        types = map doomEdNum things
        in do
            if (length sp) > 0 then do
                putStrLn "      Singleplayer:"
                thingStats' sp
                else return ()

            if (length mp) > 0 then do
            putStrLn "      Multiplayer:"
            thingStats' mp
                else return ()

thingStats' :: [Thing] -> IO ()
thingStats' things = let
        foo = count (map (fromIntegral.doomEdNum) things) :: [(Int, Int)]
        bar = map (\(tid,count)->(thingName tid (M.lookup tid T.things), count)) foo
        baz = map (\(k,v) -> "       \"" ++ k ++ "\": " ++ (show v)) bar
    in do
        mapM_ putStrLn baz
    where
        thingName :: Int -> Maybe String -> String
        thingName _ (Just x) = x
        thingName i Nothing = show i

mapSplit :: [DirEnt] -> ([DirEnt],[DirEnt])
mapSplit [] = ([],[])
mapSplit (x:xs) = let maplumps' = map prepare maplumps
                      pred = ((`elem` maplumps') . prepare . LC.unpack . lumpName)
                  in (x : takeWhile pred xs, dropWhile pred xs)

-- given a list of dirents, where head is the map label, take only those
-- lumps which are related to that map
-- XXX not in use!
mapLumps :: [DirEnt] -> [DirEnt]
mapLumps [] = []
mapLumps (x:xs) = let maplumps' = map prepare maplumps
                  in x : takeWhile ((`elem` maplumps') . prepare . LC.unpack . lumpName) xs
