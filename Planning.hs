
{-# LANGUAGE TemplateHaskell #-}

module Planning where

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit

import Set


data UnsureAbout a = Definitely a | Perhaps a
  deriving (Show, Eq)

data Availability = MustDo | CanDo | WantsToAvoid | CannotDo
  deriving (Show, Eq)

data Preferences = Preferences [(Counter, Availability)]
  deriving (Show, Eq)

data Slot = Slot Integer String Preferences
  deriving (Show, Eq)

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

case_canDoEmergencyListed = canDo prefs (emergencyCounter "Bob") @? ""
  where prefs = (Preferences [(emergencyCounter "Bob", MustDo)])

predicate @/? message = not predicate @? message

case_canDoEmergencyUnlisted = canDo prefs (emergencyCounter "Bob") @/? ""
  where prefs = (Preferences [(emergencyCounter "Bob", CanDo)])

case_canDoGoodPrefs = canDo prefs (normalCounter "Bob") @? ""
  where prefs = Preferences [(normalCounter "Alice", CannotDo)]

case_canDoBadPrefs = canDo prefs (normalCounter "Alice") @/? ""
  where prefs = Preferences [(normalCounter "Alice", CannotDo)]





allMustDos :: Preferences -> [Counter] -> Bool
allMustDos (Preferences ps) cs = all (`elem` cs) needed
  where needed = [c | (c, av) <- ps, mustDo av]
        mustDo MustDo = True
        mustDo _ = False





acceptable :: Slot -> [Counter] -> Bool
acceptable (Slot n t p) cs = ((length cs) == (fromIntegral n))
                             && (all (canDo p) cs)
                             && allMustDos p cs

case_acceptableEmpty = acceptable (Slot 0 "foo" allFree) [] @? ""

case_acceptableMatch = acceptable (Slot 1 "foo" allFree) [normalCounter "Alice"] @? ""

case_acceptableUnder = acceptable (Slot 2 "foo" allFree) [normalCounter "Bob"] @/? ""

case_acceptableOver = acceptable (Slot 1 "foo" allFree)
                                 [normalCounter "Alice", normalCounter "Bob"] @/? ""

case_acceptableGoodPrefs = acceptable (Slot 1 "foo" (Preferences [(normalCounter "Alice", CannotDo)])) [normalCounter "Bob"] @? ""

case_acceptableBadPrefs = acceptable (Slot 1 "foo" (Preferences [(normalCounter "Alice", CannotDo)])) [normalCounter "Alice"] @/? ""




usableRotas :: [Counter] -> [Slot] -> [Rota]
usableRotas cs ss = [ Rota assignments | assignments <- usableRotas' cs ss ]
  where usableRotas' counters [] = []
        usableRotas' counters (slot:[]) = [ [(slot, cs)] | cs <- validFor slot counters ]
        usableRotas' counters (slot:slots) = [ (slot, cs):others
                                             | cs <- validFor slot counters
                                             , others <- usableRotas' counters slots ]
        validFor slot counters = filter (acceptable slot) (powerset counters)

case_noUsableRotas = 0 @=? length (usableRotas counters slots)
  where counters = [normalCounter "Alice", normalCounter "Bob"]
        slots = [Slot 5 "1st January" allFree, Slot 2 "2nd January" allFree]

case_usableRotas = 2 @=? length (usableRotas counters slots)
  where counters = [normalCounter "Alice", normalCounter "Bob"]
        slots = [Slot 1 "1st January" allFree, Slot 2 "2nd January" allFree]

case_usableRotasBlankSlots = 1 @=? length (usableRotas counters slots)
  where counters = [normalCounter "Alice", normalCounter "Bob"]
        slots = [Slot 2 "Two People" allFree, Slot 0 "Nobody Needed" allFree]



slotsToFill :: Rota -> Int
slotsToFill (Rota slots) = length slots



countingSets :: Rota -> [[Counter]]
countingSets (Rota slotpairs) = map countingSet slotpairs
  where countingSet (_slot, counters) = counters



pairTogether :: Rota -> Counter -> Counter -> Int
pairTogether r c1 c2 = length (filter containsThePair (countingSets r))
  where containsThePair cs = (c1 `elem` cs) && (c2 `elem` cs)



tests :: TestTree
tests = $(testGroupGenerator)
