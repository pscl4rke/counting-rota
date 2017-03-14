

module Scoring where





-- ROADMAP

-- I need a weighting mechanism to make some strikes more influencial
-- than others.

-- Possibly there could be value in a ScoreBoard type that tracks all the
-- subscores, instead of immediately combining them into one.  This could
-- be a useful thing to display, especially when debugging why some rotas
-- appear in unexpected orders.





import  Data.List (intersect)

import Planning

import Test.HUnit

pairOff :: [a] -> [(a, a)]
pairOff [] = []
pairOff (x:[]) = []
pairOff (x:y:ys) = (x, y):(pairOff (y:ys))

test_pairOffEmpty = TestCase $ assertEqual "PairOff Empty List"
                                    ([] :: [(Bool, Bool)])
                                    (pairOff [])

test_pairOffSingle = TestCase $ assertEqual "PairOff Single Element"
                                    []
                                    (pairOff ["foo"])

test_pairOffDouble = TestCase $ assertEqual "PairOff Double Element"
                                    [("foo", "bar")]
                                    (pairOff ["foo", "bar"])

test_pairOffMany = TestCase $ assertEqual "PairOff Many Elements"
                                    [(0, 1), (1, 4), (4, 9), (9, 16)]
                                    (pairOff [0, 1, 4, 9, 16])




countersInTwice :: (Slot, [Counter]) -> (Slot, [Counter]) -> Int
countersInTwice (_, cs1) (_, cs2) = length $ intersect cs1 cs2 -- FIXME does this work




twoWeeksInARow :: Rota -> Int
twoWeeksInARow (Rota slotsAndCounters) = sum  [ countersInTwice weekA weekB
                                              | (weekA, weekB) <- pairOff slotsAndCounters]




idealMaxService :: [Counter] -> Rota -> Int
idealMaxService cs (Rota slotsAndCounters) = (fromIntegral totalGaps) `div` totalCounters
  where totalGaps = sum [ n | (Slot n _ _, _) <- slotsAndCounters ]
        totalCounters = length cs



aboveAndBeyond :: [Counter] -> Rota -> Int
--aboveAndBeyond allCounters (Rota xs) = sum $ map aboveInSlot xs
--  where aboveInSlot (_, counters) = sum $ map aboveForCounter counters
--        aboveForCounter counter
aboveAndBeyond allCounters rota = sum $ map (aboveForCounter rota) allCounters
  where aboveForCounter rota counter = 0 `max` (timesServed counter rota)
        timesServed counter (Rota slotsAndCounters) = length [ slot
                                     | (slot, counters) <- slotsAndCounters
                                     , counter `elem` counters ]




overallStrikes :: [Counter] -> Rota -> Int
overallStrikes cs r = (twoWeeksInARow r) + (aboveAndBeyond cs r)
