
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

r :: String -> Preferences
r line = case (splitOn "|" line) of
    [left, right] -> ((q left) `without` (p right))
    _ -> error ("Invalid columns: '" ++ line ++ "'")

s :: String -> Slot
s line = case (splitOn "|" line) of
    [date, yes, no] ->
        let prefs = (q yes) `without` (p no) in
        let countersneeded = 2 in -- FIXME
        Slot countersneeded (strip date) prefs
    _ -> error ("Invalid columns: '" ++ line ++ "'")

slots =
    [ s "     1st Jan    | Carol    |               "
    , s "     2nd Jan    |          |               "
    , s "     3rd Jan    |          |               "
    , s "     4th Jan    |          | Dave, Eve     "
    , s "     5th Jan    |          | Alice         "
    , s "     6th Jan    |          | Bob?, Dave    "
    , s "     7th Jan    |          |               "
    ]
