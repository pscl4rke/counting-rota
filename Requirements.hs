
module Requirements where

import Data.List.Split (splitOn)

import Parsing
import Planning



alice =     Counter True  "Alice"
bob =       Counter True  "Bob"
carol =     Counter False "Carol"
dave =      Counter True  "Dave"
eve =       Counter True  "Eve"

counters =
    [ alice
    , bob
    , carol
    , dave
    , eve
    ]

s :: String -> Slot
s line = case (parseLine counters line) of
    Left message -> error message
    Right slot -> slot

isOnSwitch x  = (x == ">>>>> ON")
isOffSwitch x = (x == "<<<<< OFF")

onlyOn :: Bool -> [String] -> [String]
onlyOn _ [] = []
onlyOn False (x:xs) | isOffSwitch x = onlyOn False xs
                    | isOnSwitch x  = onlyOn True xs
                    | otherwise     = onlyOn False xs
onlyOn True (x:xs)  | isOffSwitch x = onlyOn False xs
                    | isOnSwitch x  = onlyOn True xs
                    | otherwise     = x:(onlyOn True xs)


loadSlotsFromLines :: [String] -> [Slot]
loadSlotsFromLines inputLines =
    let linesToConsider = onlyOn False inputLines
    in map s linesToConsider


loadSlotsFromPath :: String -> IO [Slot]
loadSlotsFromPath path = do
    textFromFile <- readFile path
    return $ loadSlotsFromLines (splitOn "\n" textFromFile)
