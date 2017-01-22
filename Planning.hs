
module Planning where

import Set

import Test.HUnit




data Slot = Slot Integer String
  deriving Show

data Counter = Counter String
  deriving Show

data Rota = Rota [(Slot, [Counter])]
  deriving Show




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
usableRotas cs ss = [ Rota assignments | assignments <- usableRotas' cs ss ]
  where usableRotas' counters [] = []
        usableRotas' counters (slot:[]) = [ [(slot, cs)] | cs <- validFor slot counters ]
        usableRotas' counters (slot:slots) = [ (slot, cs):others
                                             | cs <- validFor slot counters
                                             , others <- usableRotas' counters slots ]
        validFor slot counters = filter (acceptable slot) (powerset counters)

test_noUsableRotas = TestCase $ assertEqual "No Usable Rotas"
                            0
                            (length (usableRotas counters slots))
  where counters = [Counter "Alice", Counter "Bob"]
        slots = [Slot 5 "1st January", Slot 2 "2nd January"]

test_usableRotas = TestCase $ assertEqual "2 Usable Rotas"
                            2
                            (length (usableRotas counters slots))
  where counters = [Counter "Alice", Counter "Bob"]
        slots = [Slot 1 "1st January", Slot 2 "2nd January"]
