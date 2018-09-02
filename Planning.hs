
module Planning where

import Set

import Test.Tasty.HUnit


data UnsureAbout a = Definitely a | Perhaps a
  deriving (Show, Eq)

data Availability = MustDo | CanDo | WantsToAvoid | CannotDo
  deriving (Show, Eq)

data Preferences = Preferences [(Counter, Availability)]
  deriving Show

data Slot = Slot Integer String Preferences
  deriving Show

data Counter = Counter Bool String
  deriving (Show, Eq)

data Rota = Rota [(Slot, [Counter])]
  deriving Show


normalCounter :: String -> Counter
normalCounter name = Counter True name


emergencyCounter :: String -> Counter
emergencyCounter name = Counter False name


blankRota :: Integer -> [String] -> Rota
blankRota needed names = Rota [((defaultSlot name), []) | name <- names]
  where defaultSlot name = Slot needed name (Preferences [])


allFree = Preferences []


allExcept :: [Counter] -> Preferences
allExcept cs = Preferences [(c, CannotDo) | c <- cs]


without :: [Counter] -> [UnsureAbout Counter] -> Preferences
without musts unsurenots = Preferences (mustpairs ++ unsurenotpairs)
  where mustpairs = [(c, MustDo) | c <- musts]
        unsurenotpairs = [(f uc) | uc <- unsurenots]
        f (Definitely c) = (c, CannotDo)
        f (Perhaps c) = (c, WantsToAvoid)


canDo :: Preferences -> Counter -> Bool
canDo (Preferences ps) c = case c of
    Counter True _ -> canDoNormal ps c
    Counter False _ -> canDoEmergency ps c
  where canDoNormal [] c = True
        canDoNormal ((c', p'):ps) c = if (c' == c)
                                 then (if (p' == CannotDo) then False else True)
                                 else (canDoNormal ps c)
        canDoEmergency [] c = False
        canDoEmergency ((c', p'):ps) c = if (c' == c)
                                         then (if (p' == MustDo) then True else False)
                                         else (canDoEmergency ps c)

test_canDoEmergencyListed = testCase "CanDo Emergency Listed" $ assertEqual
                                    "Error occurred"
                                    True
                                    (canDo (Preferences [(emergencyCounter "Bob", MustDo)]) (emergencyCounter "Bob"))

test_canDoEmergencyUnlisted = testCase "CanDo Emergency Unlisted" $ assertEqual
                                    "Error occurred"
                                    False
                                    (canDo (Preferences [(emergencyCounter "Bob", CanDo)]) (emergencyCounter "Bob"))

test_canDoGoodPrefs = testCase "CanDo Good Prefs" $ assertEqual
                                    "Error occurred"
                                    True
                                    (canDo (Preferences [(normalCounter "Alice", CannotDo)]) (normalCounter "Bob"))

test_canDoBadPrefs = testCase "CanDo Bad Prefs" $ assertEqual
                                    "Error occurred"
                                    False
                                    (canDo (Preferences [(normalCounter "Alice", CannotDo)]) (normalCounter "Alice"))





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
                                    (acceptable (Slot 1 "foo" allFree) [normalCounter "Alice"])

test_acceptableUnder = testCase "Acceptable Under" $ assertEqual
                                    "Error occurred"
                                    False
                                    (acceptable (Slot 2 "foo" allFree) [normalCounter "Bob"])

test_acceptableOver = testCase "Acceptable Over" $ assertEqual
                                    "Error occurred"
                                    False
                                    (acceptable (Slot 1 "foo" allFree)
                                        [normalCounter "Alice", normalCounter "Bob"])

test_acceptableGoodPrefs = testCase "Acceptable Good Prefs" $ assertEqual
                                        "Error occurred"
                                        True
                                        (acceptable (Slot 1 "foo" (Preferences [(normalCounter "Alice", CannotDo)])) [normalCounter "Bob"])

test_acceptableBadPrefs = testCase "Acceptable Bad Prefs" $ assertEqual
                                        "Error occurred"
                                        False
                                        (acceptable (Slot 1 "foo" (Preferences [(normalCounter "Alice", CannotDo)])) [normalCounter "Alice"])




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
  where counters = [normalCounter "Alice", normalCounter "Bob"]
        slots = [Slot 5 "1st January" allFree, Slot 2 "2nd January" allFree]

test_usableRotas = testCase "2 Usable Rotas" $ assertEqual
                            "Error occurred"
                            2
                            (length (usableRotas counters slots))
  where counters = [normalCounter "Alice", normalCounter "Bob"]
        slots = [Slot 1 "1st January" allFree, Slot 2 "2nd January" allFree]



slotsToFill :: Rota -> Int
slotsToFill (Rota slots) = length slots



countingSets :: Rota -> [[Counter]]
countingSets (Rota slotpairs) = map countingSet slotpairs
  where countingSet (_slot, counters) = counters



pairTogether :: Rota -> Counter -> Counter -> Int
pairTogether r c1 c2 = length (filter containsThePair (countingSets r))
  where containsThePair cs = (c1 `elem` cs) && (c2 `elem` cs)
