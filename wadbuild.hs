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

data WadInfoCommand = WadInfoLabel String | WadInfoJunk String | SomethingElse
    | IWAD | PWAD
    deriving (Show,Eq)

wadInfoFile = do
    magic  <- wadInfoHeader
    result <- option [] wadInfoBody
    eof
    return $ magic:(filter ((/=) SomethingElse) result)

wadInfoHeader = do
    skipMany $ (wadInfoComment <|> emptyLine) >> newline
    wadMagic

wadInfoBody = do
    newline -- after the header
    wadInfoLine `sepEndBy` newline

wadInfoLine = wadInfoLabel <|> wadInfoComment <|> wadInfoJunk <|> emptyLine

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

wadInfoLabel = do
    string "label "
    label <- many1 $ satisfy $ (flip elem) ['!'..'~']
    return $ WadInfoLabel label

-- inline junk, QP-encoded.
wadInfoJunk = do
    string "junk "
    junk <- many1 $ satisfy $ (flip elem) ['!'..'~']
    return $ WadInfoJunk junk

------------------------------------------------------------------------------
-- test data

parsePatch :: String -> Either ParseError [WadInfoCommand]
parsePatch x = parse wadInfoFile "" x

-- good test data
-- XXX: need to firm these up with comparisons to literal parse trees
test_wad_marker_only     = (assertRight . parsePatch) "IWAD"
test_wad_marker_newline  = (assertRight . parsePatch) "IWAD\n"
test_pwad_marker_newline = (assertRight . parsePatch) "PWAD\n"
test_pre_marker_comment  = (assertRight . parsePatch) "#comment\nIWAD\n"
test_post_marker_comment = (assertRight . parsePatch) "PWAD\n#comment"
test_multi_empty_lines   = (assertRight . parsePatch) "IWAD\n\n"
test_pre_post_empty_line = (assertRight . parsePatch) "\nIWAD\n\n"
test_simple_label        = (assertRight . parsePatch) "PWAD\nlabel MAP01"
test_duplicate_labels    = (assertRight . parsePatch) "PWAD\nlabel A\nlabel A"
test_junk                = (assertRight . parsePatch) "PWAD\njunk 01234567"

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

main = htfMain htf_thisModulesTests
