
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

textIn = "\
\>>>>> ON\n\
\|     1st Jan    | Carol    |               |\n\
\|     2nd Jan    |          |               |\n\
\| {X} 3rd Jan    |          |               |\n\
\|     4th Jan    |          | Dave, Eve     |\n\
\|     5th Jan    |          | Alice         |\n\
\|     6th Jan    |          | Bob?, Dave    |\n\
\| {3} 7th Jan    |          |               |\n\
\<<<<< OFF\n\
\ "


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


textLines = splitOn "\n" textIn

loadSlots :: IO [Slot]
loadSlots = return $ map s $ onlyOn False textLines
