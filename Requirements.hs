
module Requirements where

import Planning



allExcept :: [Counter] -> Preferences
allExcept cs = Preferences [(c, CannotDo) | c <- cs]

without :: [Counter] -> [Counter] -> Preferences
without musts cannots = Preferences (mustpairs ++ cannotpairs)
  where mustpairs = [(c, MustDo) | c <- musts]
        cannotpairs = [(c, CannotDo) | c <- cannots]



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
    [ Slot 2 "1st Jan" ([carol] `without` [])
    , Slot 1 "2nd Jan" allFree
    , Slot 2 "3rd Jan" allFree
    , Slot 2 "4th Jan" (allExcept [dave, eve])
    , Slot 3 "5th Jan" allFree
    , Slot 2 "6th Jan" allFree
    , Slot 2 "7th Jan" allFree
    ]
