
module Requirements where

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

slots =
    [ Slot 2 "1st Jan" allFree
    , Slot 1 "2nd Jan" allFree
    , Slot 2 "3rd Jan" allFree
    , Slot 2 "4th Jan" allFree
    , Slot 3 "5th Jan" allFree
    , Slot 2 "6th Jan" allFree
    , Slot 2 "7th Jan" allFree
    ]
