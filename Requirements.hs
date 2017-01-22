
module Requirements where

import Planning

counters =
    [ Counter "Alice"
    , Counter "Bob"
    , Counter "Carol"
    , Counter "Dave"
    , Counter "Eve"
    ]

slots =
    [ Slot 2 "1st Jan" allFree
    , Slot 1 "2nd Jan" allFree
    , Slot 2 "3rd Jan" allFree
    , Slot 2 "4th Jan" allFree
    , Slot 3 "5th Jan" allFree
    ]
