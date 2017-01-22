
module Planning where

import Set

import Test.HUnit

data Slot = Slot Integer String
  deriving Show

data Counter = Counter String
  deriving Show

data Rota = Rota [(Slot, [Counter])]
  deriving Show

allRotas :: [Counter] -> [Slot] -> [Rota]
allRotas cs ss = [ Rota assignments | assignments <- allRotas' cs ss ]
  where allRotas' counters [] = []
        allRotas' counters (slot:[]) = [ [(slot, cs)] | cs <- powerset counters ]
        allRotas' counters (slot:slots) = [ (slot, cs):others
                                          | cs <- powerset counters
                                          , others <- allRotas' counters slots ]

test_allRotas = TestCase $ assertEqual "allRotas"
                            16
                            (length (allRotas counters slots))
  where counters = [Counter "Alice", Counter "Bob"]
        slots = [Slot 5 "1st January", Slot 2 "2nd January"]

acceptable :: Slot -> [Counter] -> Bool
acceptable (Slot n t) cs = (length cs) == (fromIntegral n)

test_acceptableMatch = TestCase $ assertEqual "Acceptable Good"
                                    True
                                    (acceptable (Slot 1 "foo") [Counter "Alice"])

test_acceptableUnder = TestCase $ assertEqual "Acceptable Under"
                                    False
                                    (acceptable (Slot 2 "foo") [Counter "Bob"])

test_acceptableOver = TestCase $ assertEqual "Acceptable Over"
                                    False
                                    (acceptable (Slot 1 "foo") [Counter "Alice", Counter "Bob"])

usableRotas :: [Counter] -> [Slot] -> [Rota]
usableRotas cs ss = filter validNumber $ allRotas cs ss
  where validNumber (Rota assignments) = all validAssignment assignments
        validAssignment (slot, counters) = acceptable slot counters

test_usableRotas = TestCase $ assertEqual "usableRotas"
                            2
                            (length (usableRotas counters slots))
  where counters = [Counter "Alice", Counter "Bob"]
        slots = [Slot 1 "1st January", Slot 2 "2nd January"]
