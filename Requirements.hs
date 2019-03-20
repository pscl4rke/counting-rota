
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

parseLine :: [Counter] -> String -> Either String Slot
parseLine counters line = case (splitOn "|" line) of
    [_, spacesAndDate, yes, no, _] -> do
        yes_prefs <- parseListOfCounter counters yes
        no_prefs <- parseListOfUnsureAboutCounter counters no
        let prefs = yes_prefs `without` no_prefs
        let defaultSpaces = 2  -- FIXME duplicating rota.hs
        (spaces, date) <- splitSpacesAndDate defaultSpaces spacesAndDate
        Right $ Slot spaces (strip date) prefs
    _ -> Left ("Invalid columns: '" ++ line ++ "'")

s :: String -> Slot
s line = case (parseLine counters line) of
    Left message -> error message
    Right slot -> slot

textIn = "\
\|     1st Jan    | Carol    |               |\n\
\|     2nd Jan    |          |               |\n\
\| {X} 3rd Jan    |          |               |\n\
\|     4th Jan    |          | Dave, Eve     |\n\
\|     5th Jan    |          | Alice         |\n\
\|     6th Jan    |          | Bob?, Dave    |\n\
\| {3} 7th Jan    |          |               |\
\ "

textLines = splitOn "\n" textIn
slots = map s textLines
