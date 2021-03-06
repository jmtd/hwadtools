import System.Environment (getArgs)
import System.IO
import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC -- unpack
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Binary.Get
import Data.List (sortBy)
import System.FilePath --(</>)
import Codec.Binary.QuotedPrintable (encode)
import Data.Int (Int32)
import Data.Binary (decode)

import Wad


usage = do
    error "usage: blah"

getHandle :: IO (Handle, String)
getHandle = do
    args <- getArgs
    handle <- if length args /= 2
              then usage
              else openFile (head args) ReadMode
    hSetBinaryMode handle True
    return (handle, args !! 1)

{-
  what info do we need to record in wadinfo.txt in order to faithfully recreate the WAD?
  can we rely on the directory entries being in the right order (ascending offsets?) no.
  thus we need to write the offsets.
  can we infer the lump size from the file on disk? yes... except, if we have some janky
  overlapping offsets and a resource is changed later... can we handle that situation?
  so we just need rawname (since we want reproducibility) and offset, it seems.
-}

-- strip trailing \0 and QP-encode the remaining string
myEncode :: L.ByteString -> String
myEncode x = BC.unpack $ encode $ L.toStrict $ myEncode' x

myEncode' :: L.ByteString -> L.ByteString
myEncode' bs | bs == L.empty = bs
             | head == '\0' && tail == L.empty = L.empty
             | otherwise = head `LC.cons` tail
    where
        head = LC.head bs
        tail = myEncode' $ L.tail bs

wadInfoEntry :: DirEnt -> String
wadInfoEntry (DirEnt offs size rawname) =
    if 0 == size
        then "label " ++ encname
        else "lump " ++ encname ++ " " ++ (show offs) ++ " " ++ "path/to/lump/XXX"
    where
        encname = myEncode rawname

writewadInfo :: L.ByteString -> [DirEnt] -> FilePath -> IO ()
writewadInfo wad dirents outdir = do
    fh <- openFile (outdir </> "wadinfo.txt") WriteMode

    let header = decode wad :: WadHeader
    case (LC.unpack $ magic header) of
        "IWAD" -> hPutStr fh "IWAD\n"
        "PWAD" -> hPutStr fh "PWAD\n"
        _ -> return () -- XXX: invalid magic

    hPutStr fh $ unlines $ map wadInfoEntry dirents
    possibleJunkEntries wad fh dirents
    hClose fh

-- XXX: sort the dirents by offset to do a recursive walk for holes
possibleJunkEntries :: L.ByteString -> Handle -> [DirEnt] -> IO ()
possibleJunkEntries wad fh dirents = do
    possibleJunkEntries' wad fh 12 (sortBy sortfn dirents) where
        sortfn (DirEnt x _ _) (DirEnt y _ _) = compare x y

possibleJunkEntries' :: L.ByteString -> Handle -> Int32 -> [DirEnt] -> IO ()
possibleJunkEntries' _ _ _ [] = return ()
possibleJunkEntries' wad fh fpos ((DirEnt offs size name):ds) = do
    if   fpos /= offs
        then hPutStr fh $ "junk " ++ (show fpos) ++ " " ++ enc ++ "\n"
            else return ()
    possibleJunkEntries' wad fh (offs+size) ds
    where
        hole = offs - fpos -- XXX possibly negative?
        junk = L.take (fromIntegral hole) (L.drop (fromIntegral fpos) wad) -- fromIntegral hole -> Int32 -> Int64
        enc  = BC.unpack $ encode $ L.toStrict $ junk -- type: B.ByteString

-- XXX problem: rawname is being truncated at the first \0 here so we need a mapping fn instead
--      in isolation, we could use QP encoding for the filename
-- XXX another problem: lump names could clash, we need to distinguish them somehow
--      this is the harder problem
-- XXX another problem: we need to recognise MAP markers and bundle such lumps together
--      oh wait maybe this is the hard one
writeLump :: L.ByteString -> FilePath -> DirEnt -> IO ()
writeLump input outdir (DirEnt offs' size' rawname) =
    if size == 0
    then -- label; skip
        return ()
    else
        L.writeFile fn lump
    where
        fn = outdir </> (LC.unpack rawname)
        lump = L.take size (L.drop offs input)
        size = fromIntegral size'
        offs = fromIntegral offs'

writeLumps :: L.ByteString -> FilePath -> [DirEnt] -> IO ()
writeLumps input outdir dirents = mapM_ (writeLump input outdir) dirents

extractWad :: Handle -> String -> IO ()
extractWad handle outdir = do
    input  <- L.hGetContents handle
    let dirents = wadDirEnts input
    writewadInfo input dirents outdir
    writeLumps input outdir dirents

main = do
    (handle, outdir) <- getHandle
    createDirectoryIfMissing True outdir
    extractWad handle outdir
