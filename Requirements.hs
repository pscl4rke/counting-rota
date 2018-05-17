
module Requirements where

import Data.List.Split (splitOn)

import Parsing
import Planning



alice =     Counter "Alice"
bob =       Counter "Bob"
carol =     Counter "Carol"
dave =      Counter "Dave"
eve =       Counter "Eve"

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

slots =
    [ Slot 2 "1st Jan" (r "Carol    |               ")
    , Slot 1 "2nd Jan" (r "         |               ")
    , Slot 2 "3rd Jan" (r "         |               ")
    , Slot 2 "4th Jan" (r "         | Dave, Eve     ")
    , Slot 3 "5th Jan" (r "         | Alice         ")
    , Slot 2 "6th Jan" (r "         | Bob?, Dave    ")
    , Slot 2 "7th Jan" (r "         |               ")
    ]
