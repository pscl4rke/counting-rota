
module Set where

import Data.List (sort)

import Test.Tasty.HUnit

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = (powersetWith x xs) ++ (powersetWithout x xs)
  where powersetWith x xs = [ x:ys | ys <- powerset xs ]
        powersetWithout x xs = powerset xs

test_powersetEmpty = testCase "Empty input" $ assertEqual
                                    "Error occurred"
                                    [[]]
                                    (powerset [] :: [[Integer]])

test_powersetOne = testCase "One Item" $ assertEqual
                                    "Error occurred"
                                    [[], [8]]
                                    (sort (powerset [8]))

test_powersetThree = testCase "Three Items" $ assertEqual
                                    "Error occurred"
                                    [[], ["a"], ["a", "bb"], ["a", "bb", "ccc"],
                                     ["a", "ccc"], ["bb"], ["bb", "ccc"], ["ccc"]]
                                    (sort (powerset ["a", "bb", "ccc"]))
