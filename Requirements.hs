
module Requirements where

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

slots =
    [ Slot 2 "1st Jan" (q "Carol" `without` p "")
    , Slot 1 "2nd Jan" allFree
    , Slot 2 "3rd Jan" allFree
    , Slot 2 "4th Jan" (q "" `without` p "Dave, Eve")
    , Slot 3 "5th Jan" (q "" `without` p "  Alice")
    , Slot 2 "6th Jan" (q "   " `without` p "Bob?, Dave")
    , Slot 2 "7th Jan" allFree
    ]
