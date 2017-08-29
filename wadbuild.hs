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

data WadInfoCommand = WadInfoLabel String | SomethingElse
    | IWAD | PWAD
    deriving (Show,Eq)

-- it would be nice to extend this to support parsing the wadMagic before any other commands 
-- with comments/whitespace interspersed freely around them
wadInfoFile = do
    skipMany ((wadInfoComment <|> emptyLine) >> eol)
    magic <- wadMagic
    eol
    result <- wadInfoLine `sepEndBy` eol
    eof
    return (magic:(filter ((/=) SomethingElse) result))

eol = char '\n'

wadInfoLine = wadInfoLabel <|> wadInfoComment <|> emptyLine

-- can we return nowt instead and do away with emptyLine?
isSpaceNotNewLine x = (isSpace x) && (x /= '\n')

emptyLine = do
    skipMany (satisfy isSpaceNotNewLine)
    lookAhead eol
    return SomethingElse

wadMagic = iwadMagic <|> pwadMagic

iwadMagic = do
    string "IWAD"
    return IWAD

pwadMagic = do
    string "PWAD"
    return PWAD

wadInfoComment = do
    char '#'
    many (noneOf "\n")
    --many1 anyChar -- seems we still need to avoid eating a \n here
    return SomethingElse -- for type checking, but we want to ignore these really

wadInfoLabel = do
    string "label "
    label <- many1 (noneOf "\n")
    return $ WadInfoLabel label
    -- error out if >8?

parsePatch :: String -> Either ParseError [WadInfoCommand]
parsePatch x = parse wadInfoFile "" x

pp2 :: Either ParseError [WadInfoCommand] -> [WadInfoCommand]
pp2 (Left _) = error "uh oh"
pp2 (Right xs) = filter ((/=) SomethingElse) xs

-- good test data
-- XXX: need to firm these up with comparisons to literal parse trees
test_0 = (assertRight . parsePatch)  "IWAD\n"             -- simplest
test_1 = (assertRight . parsePatch)  "PWAD\n"
test_2 = (assertRight . parsePatch)  "#comment\nIWAD\n"   -- pre-comment
test_3 = (assertRight . parsePatch)  "PWAD\n#comment"     -- post-comment
test_6 = (assertRight . parsePatch)  "IWAD\n"             -- free whitespace
test_7 = (assertRight . parsePatch)  "IWAD\n\n"
test_8 = (assertRight . parsePatch)  "\nIWAD\n\n"
test_9 = (assertRight . parsePatch)  "PWAD\nlabel MAP01"  -- valid label (<8 length)
test_10 = (assertRight . parsePatch) "PWAD\nlabel MAP01\nlabel MAP01"  -- valid labels (duplicated)
test_11 = (assertRight . parsePatch) "PWAD\nlabel 01234567" -- valid label (==8 length)
test_12 = (assertRight . parsePatch) "PWAD\nlabel 01234567 " -- valid label (whitespace suffix)
test_13 = (assertRight . parsePatch) "PWAD\nlabel 01234567#" -- valid label (empty comment suffix)
test_14 = (assertRight . parsePatch) "PWAD\nlabel 01234567# comment" -- valid label (comment suffix)

-- bad test data
test_4 = (assertLeft . parsePatch)  "PWAD#comment"     -- suffixed comment not supported
test_5 = (assertLeft . parsePatch)  "PWAD #comment"    -- suffixed whitespace (and comment)
test_15 = (assertLeft . parsePatch) "IWAD\n# blah\nIWAD" -- too many magics -- failing
test_16 = (assertLeft . parsePatch) "IWAD\nPWAD"         -- too many magics -- failing
test_17 = (assertLeft . parsePatch) "JWAD"               -- invalid magic
test_18 = (assertLeft . parsePatch) ""                   -- missing commands -- failing
test_19 = (assertLeft . parsePatch) "\n" -- failing
test_20 = (assertLeft . parsePatch) "# nothing here" -- failing
test_21 = (assertLeft . parsePatch) "label foo"          -- missing magic -- failing
test_22 = (assertLeft . parsePatch) "label foo\nIWAD"    -- magic not first -- failing
test_23 = (assertLeft . parsePatch) "PWAD\nlabel 123456789" -- invalid label (>8 length) -- failing
test_24 = (assertLeft . parsePatch) "    IWAD"
test_25 = (assertLeft . parsePatch) "IWAD" -- missing newline suffix

main = htfMain htf_thisModulesTests
