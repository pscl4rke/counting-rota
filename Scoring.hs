

module Scoring where





import  Data.List (intersect)

import Planning

import Test.HUnit



data Scorecard = Scorecard
    { scoreTwoWeeksInARow :: Int
    , scoreAboveAndBeyond :: Int
    , scorePartnerVariety :: Int
    }



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
  where aboveForCounter rota counter = 0 `max` ((timesServed counter rota)
                                                - (idealMaxService allCounters rota))
        timesServed counter (Rota slotsAndCounters) = length [ slot
                                     | (slot, counters) <- slotsAndCounters
                                     , counter `elem` counters ]




ideally :: [Counter] -> Rota -> Int
ideally cs r = 1 + ((slotsToFill r) `div` ((length cs) * ((length cs) - 1)))



partnerVariety :: [Counter] -> Rota -> Int
partnerVariety cs r = (sum [over r c1 c2 | c1 <- cs, c2 <- cs]) `div` 2
  where over r c1 c2 = 0 `max` ((pairTogether r c1 c2) - (ideally cs r))




scorecard :: [Counter] -> Rota -> Scorecard
scorecard cs r = Scorecard {
                    scoreTwoWeeksInARow = (twoWeeksInARow r),
                    scoreAboveAndBeyond = 2 * (aboveAndBeyond cs r),
                    scorePartnerVariety = (partnerVariety cs r)
                    }



scoreTotal :: Scorecard -> Int
scoreTotal (Scorecard a b c) = a + b + c


overallStrikes :: [Counter] -> Rota -> Int
overallStrikes cs r = scoreTotal $ scorecard cs r
