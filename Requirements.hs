
module Requirements where

import Parsing
import Planning



allExcept :: [Counter] -> Preferences
allExcept cs = Preferences [(c, CannotDo) | c <- cs]

without :: [Counter] -> [UnsureAbout Counter] -> Preferences
without musts unsurenots = Preferences (mustpairs ++ unsurenotpairs)
  where mustpairs = [(c, MustDo) | c <- musts]
        unsurenotpairs = [(f uc) | uc <- unsurenots]
        f (Definitely c) = (c, CannotDo)
        f (Perhaps c) = (c, WantsToAvoid)




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

p :: String -> Counter
p name = case (parseCounter counters name) of
    Just counter -> counter
    Nothing -> error ("Cannot parse " ++ name)

slots =
    [ Slot 2 "1st Jan" ([p "Carol"] `without` [])
    , Slot 1 "2nd Jan" allFree
    , Slot 2 "3rd Jan" allFree
    , Slot 2 "4th Jan" (allExcept [p "Dave", p "Eve"])
    , Slot 3 "5th Jan" ([] `without` [(Definitely (p "Alice"))])
    , Slot 2 "6th Jan" ([] `without` [(Perhaps (p "Bob"))])
    , Slot 2 "7th Jan" allFree
    ]
