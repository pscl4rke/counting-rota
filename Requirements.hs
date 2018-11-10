
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

slots =
    [ s "|     1st Jan    | Carol    |               |"
    , s "|     2nd Jan    |          |               |"
    , s "| {X} 3rd Jan    |          |               |"
    , s "|     4th Jan    |          | Dave, Eve     |"
    , s "|     5th Jan    |          | Alice         |"
    , s "|     6th Jan    |          | Bob?, Dave    |"
    , s "| {3} 7th Jan    |          |               |"
    ]
