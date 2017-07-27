
module Planning where

import Set

import Test.Tasty.HUnit



data Availability = MustDo | CanDo | WantsToAvoid | CannotDo
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

test_canDoGoodPrefs = testCase "CanDo Good Prefs" $ assertEqual
                                    "Error occurred"
                                    True
                                    (canDo (Preferences [(Counter "Alice", CannotDo)]) (Counter "Bob"))

test_canDoBadPrefs = testCase "CanDo Bad Prefs" $ assertEqual
                                    "Error occurred"
                                    False
                                    (canDo (Preferences [(Counter "Alice", CannotDo)]) (Counter "Alice"))





allMustDos :: Preferences -> [Counter] -> Bool
allMustDos (Preferences ps) cs = all (`elem` cs) needed
  where needed = [c | (c, av) <- ps, mustDo av]
        mustDo MustDo = True
        mustDo _ = False





acceptable :: Slot -> [Counter] -> Bool
acceptable (Slot n t p) cs = ((length cs) == (fromIntegral n))
                             && (all (canDo p) cs)
                             && allMustDos p cs

test_acceptableMatch = testCase "Acceptable Good" $ assertEqual
                                    "Error occurred"
                                    True
                                    (acceptable (Slot 1 "foo" allFree) [Counter "Alice"])

test_acceptableUnder = testCase "Acceptable Under" $ assertEqual
                                    "Error occurred"
                                    False
                                    (acceptable (Slot 2 "foo" allFree) [Counter "Bob"])

test_acceptableOver = testCase "Acceptable Over" $ assertEqual
                                    "Error occurred"
                                    False
                                    (acceptable (Slot 1 "foo" allFree)
                                        [Counter "Alice", Counter "Bob"])

test_acceptableGoodPrefs = testCase "Acceptable Good Prefs" $ assertEqual
                                        "Error occurred"
                                        True
                                        (acceptable (Slot 1 "foo" (Preferences [(Counter "Alice", CannotDo)])) [Counter "Bob"])

test_acceptableBadPrefs = testCase "Acceptable Bad Prefs" $ assertEqual
                                        "Error occurred"
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

test_noUsableRotas = testCase "No Usable Rotas" $ assertEqual
                            "Error occurred"
                            0
                            (length (usableRotas counters slots))
  where counters = [Counter "Alice", Counter "Bob"]
        slots = [Slot 5 "1st January" allFree, Slot 2 "2nd January" allFree]

test_usableRotas = testCase "2 Usable Rotas" $ assertEqual
                            "Error occurred"
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
