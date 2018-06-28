module Doom.Things(things) where
-- ingested so far: pickups,monsters

import qualified Data.IntMap.Strict as IntMap

things = IntMap.fromList [
    ( 1, "player 1 start" ),
    ( 2, "player 2 start" ),
    ( 3, "player 3 start" ),
    ( 4, "player 4 start" ),
    ( 5, "bluekeycard"),
    ( 6, "yellowkeycard"),
    ( 7, "spidermastermind"),
    ( 8, "backpack"),
    ( 9, "formersergeant"),
    ( 11, "deathmatchstart" ),
    ( 13, "redkeycard"),
    ( 16, "cyberdemon"),
    ( 17, "cellchargepack"),
    ( 38, "redskullkey"),
    ( 39, "yellowskullkey"),
    ( 40, "blueskullkey"),
    ( 58, "spectre"),
    ( 64, "archvile"),
    ( 65, "heavyweapondude"),
    ( 66, "revenant"),
    ( 67, "mancubus"),
    ( 68, "arachnotron"),
    ( 69, "hellknight"),
    ( 71, "painelemental"),
    ( 82, "doublebarreled"),
    ( 83, "megasphere"),
    ( 84, "wolfensteinss"),
    ( 87, "spawnspot"),
    ( 88, "bossbrain"),
    ( 89, "bossshooter"),
    ( 2001, "shotgun"),
    ( 2002, "chaingun"),
    ( 2003, "rocketlauncher"),
    ( 2004, "plasmagun"),
    ( 2005, "chainsaw"),
    ( 2006, "bfg9000"),
    ( 2007, "ammoclip"),
    ( 2008, "shotgunshells"),
    ( 2010, "arocket"),
    ( 2011, "stimpak"),
    ( 2012, "medikit"),
    ( 2013, "soulsphere"),
    ( 2014, "healthpotion"),
    ( 2015, "spiritarmor"),
    ( 2018, "greenarmor"),
    ( 2019, "bluearmor"),
    ( 2022, "invulnerability"),
    ( 2023, "berserk"),
    ( 2024, "invisibility"),
    ( 2025, "radiationsuit"),
    ( 2026, "computermap"),
    ( 2045, "goggles"),
    ( 2046, "boxofrockets"),
    ( 2047, "cellcharge"),
    ( 2048, "boxofammo"),
    ( 2049, "boxofshells"),
    ( 3001, "imp"),
    ( 3002, "demon"),
    ( 3003, "baronofhell"),
    ( 3004, "formerhuman"),
    ( 3005, "cacodemon"),
    ( 3006, "lostsoul")
    ]
