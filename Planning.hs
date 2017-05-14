
module Planning where

import Set

import Test.HUnit



data Availability = CanDo | CannotDo
  deriving (Show, Eq)

data Preferences = Preferences [(Counter, Availability)]
  deriving Show

data Slot = Slot Integer String Preferences
  deriving Show

data Counter = Counter String
  deriving (Show, Eq)

data Rota = Rota [(Slot, [Counter])]
  deriving Show


blankRota :: Integer -> [String] -> Rota
blankRota needed names = Rota [((defaultSlot name), []) | name <- names]
  where defaultSlot name = Slot needed name (Preferences [])


allFree = Preferences []


canDo :: Preferences -> Counter -> Bool
canDo (Preferences ps) c = canDo' ps c
  where canDo' [] c = True
        canDo' ((c', p'):ps) c = if (c' == c)
                                 then (if (p' == CannotDo) then False else True)
                                 else (canDo' ps c)

test_canDoGoodPrefs = TestCase $ assertEqual "CanDo Good Prefs"
                                    True
                                    (canDo (Preferences [(Counter "Alice", CannotDo)]) (Counter "Bob"))

test_canDoBadPrefs = TestCase $ assertEqual "CanDo Bad Prefs"
                                    False
                                    (canDo (Preferences [(Counter "Alice", CannotDo)]) (Counter "Alice"))





acceptable :: Slot -> [Counter] -> Bool
acceptable (Slot n t p) cs = ((length cs) == (fromIntegral n))
                             && (all (canDo p) cs)

test_acceptableMatch = TestCase $ assertEqual "Acceptable Good"
                                    True
                                    (acceptable (Slot 1 "foo" allFree) [Counter "Alice"])

test_acceptableUnder = TestCase $ assertEqual "Acceptable Under"
                                    False
                                    (acceptable (Slot 2 "foo" allFree) [Counter "Bob"])

test_acceptableOver = TestCase $ assertEqual "Acceptable Over"
                                    False
                                    (acceptable (Slot 1 "foo" allFree)
                                        [Counter "Alice", Counter "Bob"])

test_acceptableGoodPrefs = TestCase $ assertEqual "Acceptable Good Prefs"
                                        True
                                        (acceptable (Slot 1 "foo" (Preferences [(Counter "Alice", CannotDo)])) [Counter "Bob"])

test_acceptableBadPrefs = TestCase $ assertEqual "Acceptable Bad Prefs"
                                        False
                                        (acceptable (Slot 1 "foo" (Preferences [(Counter "Alice", CannotDo)])) [Counter "Alice"])




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
        slots = [Slot 5 "1st January" allFree, Slot 2 "2nd January" allFree]

test_usableRotas = TestCase $ assertEqual "2 Usable Rotas"
                            2
                            (length (usableRotas counters slots))
  where counters = [Counter "Alice", Counter "Bob"]
        slots = [Slot 1 "1st January" allFree, Slot 2 "2nd January" allFree]



slotsToFill :: Rota -> Int
slotsToFill (Rota slots) = length slots



countingSets :: Rota -> [[Counter]]
countingSets (Rota slotpairs) = map countingSet slotpairs
  where countingSet (_slot, counters) = counters



pairTogether :: Rota -> Counter -> Counter -> Int
pairTogether r c1 c2 = length (filter containsThePair (countingSets r))
  where containsThePair cs = (c1 `elem` cs) && (c2 `elem` cs)
