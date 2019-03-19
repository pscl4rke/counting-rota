
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

q :: String -> [Counter]
q names = case (parseListOfCounter counters names) of
    Right counters -> counters
    Left msg -> error ("Cannot parse counters: " ++ msg)

p :: String -> [UnsureAbout Counter]
p names = case (parseListOfUnsureAboutCounter counters names) of
    Right unsurecounters -> unsurecounters
    Left msg -> error ("Cannot parse availability: " ++ msg)

s :: String -> Slot
s line = case (splitOn "|" line) of
    [_, spacesAndDate, yes, no, _] ->
        let prefs = (q yes) `without` (p no) in
        let defaultSpaces = 2 in  -- FIXME duplicating rota.hs
        case (splitSpacesAndDate defaultSpaces spacesAndDate) of
            Left message -> error message
            Right (spaces, date) -> Slot spaces (strip date) prefs
    _ -> error ("Invalid columns: '" ++ line ++ "'")

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
