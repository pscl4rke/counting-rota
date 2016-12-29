
module Set where

import Data.List (sort)
import Test.HUnit

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = (powersetWith x xs) ++ (powersetWithout x xs)
  where powersetWith x xs = [ x:ys | ys <- powerset xs ]
        powersetWithout x xs = powerset xs

test_powersetEmpty = TestCase $ assertEqual "Empty input"
                                    (powerset [] :: [[Integer]])
                                    [[]]

test_powersetOne = TestCase $ assertEqual "One Item"
                                    (sort (powerset [8]))
                                    [[], [8]]

test_powersetThree = TestCase $ assertEqual "Three Items"
                                    (sort (powerset ["a", "bb", "ccc"]))
                                    [[], ["a"], ["a", "bb"], ["a", "bb", "ccc"],
                                     ["a", "ccc"], ["bb"], ["bb", "ccc"], ["ccc"]]
