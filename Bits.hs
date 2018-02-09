-- bits experiments

module Bits( skill1_2
           , skill3
           , skill4_5
           , ambush
           , multiplayer
           , not_dm
           , not_coop
           , friendly
           ) where

import Data.Bits
import Wad

skill1_2 :: Bits a => a -> Bool
skill1_2   a  = testBit a 0
skill3     a  = testBit a 1
skill4_5   a  = testBit a 2
ambush a      = testBit a 3
multiplayer a = testBit a 4
-- boom
not_dm a      = testBit a 5
not_coop a    = testBit a 6
-- mbf
friendly a    = testBit a 7

------------------------------------------------------------------------------

testflags = 7 :: Int -- test data
t = Thing (-1976) 0 270 1 7 -- some player1start
t2= Thing 416 672 270 2004 23 -- MAP01 plasma

main = do
    let b = skill1_2 testflags
        in do
            putStrLn.show $ b
            putStrLn.show $ multiplayer (thingflags t)
            putStrLn.show $ multiplayer (thingflags t2)
