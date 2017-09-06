{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-
   build a wad from a wadinfo file (our format that is)

   really build a Lazy ByteString sequence
    (hopefully this is powerful enough to let us build sequences with holes
     or whatnot so we can slot the directory in at the end, I mean, at the end
     of processing, not necessarily at the end of the file...)

   usage: wadbuild path/to/wadinfo.txt
    infer path/to as the root of the lumps
 -}

import Text.ParserCombinators.Parsec
import Test.Framework
import Data.Char (isSpace)

data Wad = Wad { magic   :: WadMagic
               , entries :: [WadInfoCommand]
               } deriving (Show,Eq)

data WadMagic = IWAD | PWAD deriving (Show,Eq)

data WadInfoCommand = WadInfoLabel String    -- empty directory entry e.g. MAP01
                    | WadInfoJunk Int String -- inline junk (space between data)
                    | WadInfoLump String Int String -- a lump: name, offset, path
                    | WadInfoDirectory       -- where the directory goes
                    | WadInfoSkip Int        -- a skip instruction. Must be filled in later
                    | WadInfoExtWad String   -- an external sub-wad
                    | WadInfoExtJunk String  -- external junk (too big to inline)
                    | SomethingElse          -- Ignore.
    deriving (Show,Eq)

------------------------------------------------------------------------------
-- parsing functions (building [WadInfoCommand] data structures)

wadInfoFile = do
    magic  <- wadInfoHeader
    result <- option [] wadInfoBody
    eof
    return $ Wad magic (filter ((/=) SomethingElse) result)

wadInfoHeader = do
    skipMany $ (wadInfoComment <|> emptyLine) >> newline
    wadMagic

wadInfoBody = do
    newline -- after the header
    wadInfoLine `sepEndBy` newline

wadInfoLine = try wadInfoLabel <|> wadInfoComment <|> wadInfoJunk <|> wadInfoLump <|> emptyLine

-- can we return nowt instead and do away with emptyLine?
isSpaceNotNewLine x = (isSpace x) && (x /= '\n')

emptyLine = do
    skipMany (satisfy isSpaceNotNewLine)
    lookAhead newline
    return SomethingElse

wadMagic = iwadMagic <|> pwadMagic

iwadMagic = string "IWAD" >> return IWAD
pwadMagic = string "PWAD" >> return PWAD

wadInfoComment = do
    char '#'
    many (noneOf "\n")
    return SomethingElse -- for type checking, but we want to ignore these really

-- XXX bug here. ' ' is not in this set, but QuotedPrintable won't encode it
qpEncChar = satisfy $ (flip elem) ['!'..'~']

wadInfoLabel = do
    string "label "
    label <- many1 qpEncChar
    return $ WadInfoLabel label

-- inline junk, QP-encoded.
wadInfoJunk = do
    string "junk "
    offs <- many1 digit
    spaces
    junk <- many1 qpEncChar
    return $ WadInfoJunk (read offs) junk

wadInfoLump = do
    string "lump "
    name <- many1 qpEncChar
    spaces
    offs <- many1 digit
    spaces
    path <- many1 (noneOf "\n")
    return $ WadInfoLump name (read offs) path

------------------------------------------------------------------------------
-- test data

parsePatch :: String -> Either ParseError Wad
parsePatch x = parse wadInfoFile "" x

-- good test data
test_wad_marker_only     = (assertEqual . parsePatch) "IWAD"                   $ Right $ Wad IWAD []
test_wad_marker_newline  = (assertEqual . parsePatch) "IWAD\n"                 $ Right $ Wad IWAD []
test_pwad_marker_newline = (assertEqual . parsePatch) "PWAD\n"                 $ Right $ Wad PWAD []
test_pre_marker_comment  = (assertEqual . parsePatch) "#comment\nIWAD\n"       $ Right $ Wad IWAD []
test_post_marker_comment = (assertEqual . parsePatch) "PWAD\n#comment"         $ Right $ Wad PWAD []
test_multi_empty_lines   = (assertEqual . parsePatch) "IWAD\n\n"               $ Right $ Wad IWAD []
test_pre_post_empty_line = (assertEqual . parsePatch) "\nIWAD\n\n"             $ Right $ Wad IWAD []
test_simple_label        = (assertEqual . parsePatch) "PWAD\nlabel MAP01"      $ Right $ Wad PWAD [WadInfoLabel "MAP01"]
test_duplicate_labels    = (assertEqual . parsePatch) "PWAD\nlabel A\nlabel A" $ Right $ Wad PWAD [WadInfoLabel "A", WadInfoLabel "A"]
test_junk                = (assertEqual . parsePatch) "PWAD\njunk 1234 012367" $ Right $ Wad PWAD [WadInfoJunk 1234 "012367"]
test_lump                = (assertEqual . parsePatch) "PWAD\nlump THINGS 12 some/path" $ Right $ Wad PWAD [WadInfoLump "THINGS" 12 "some/path"]

-- bad test data
test_suffixed_comment        = (assertLeft . parsePatch)  "PWAD#comment"
test_suffixed_whitespace     = (assertLeft . parsePatch)  "PWAD #comment"
test_too_many_magics_comment = (assertLeft . parsePatch) "IWAD\n# blah\nIWAD"
test_too_many_magics         = (assertLeft . parsePatch) "IWAD\nPWAD"
test_invalid_magic           = (assertLeft . parsePatch) "JWAD"
test_empty                   = (assertLeft . parsePatch) ""
test_newline_only            = (assertLeft . parsePatch) "\n"
test_comment_only            = (assertLeft . parsePatch) "# nothing here"
test_label_only              = (assertLeft . parsePatch) "label foo"
test_label_before_magic      = (assertLeft . parsePatch) "label foo\nIWAD"
test_space_before_magic      = (assertLeft . parsePatch) "    IWAD"
test_label_comment_suffix    = (assertLeft . parsePatch) "PWAD\nlabel 01234567# comment"
test_label_whitespace_suffix = (assertLeft . parsePatch) "PWAD\nlabel 01234567 "
test_empty_label             = (assertLeft . parsePatch) "PWAD\nlabel"
test_empty_label_space       = (assertLeft . parsePatch) "PWAD\nlabel "
test_empty_junk              = (assertLeft . parsePatch) "PWAD\njunk"
test_empty_junk_space        = (assertLeft . parsePatch) "PWAD\njunk "
test_missing_junk_offset     = (assertLeft . parsePatch) "PWAD\njunk 1234"

main = htfMain htf_thisModulesTests
