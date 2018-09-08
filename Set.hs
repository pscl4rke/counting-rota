
{-# LANGUAGE TemplateHaskell #-}

module Set where

import Data.List (sort)

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = (powersetWith x xs) ++ (powersetWithout x xs)
  where powersetWith x xs = [ x:ys | ys <- powerset xs ]
        powersetWithout x xs = powerset xs

case_powersetEmpty = assertEqual
                                    "Error occurred"
                                    [[]]
                                    (powerset [] :: [[Integer]])

case_powersetOne = assertEqual
                                    "Error occurred"
                                    [[], [8]]
                                    (sort (powerset [8]))

case_powersetThree = assertEqual
                                    "Error occurred"
                                    [[], ["a"], ["a", "bb"], ["a", "bb", "ccc"],
                                     ["a", "ccc"], ["bb"], ["bb", "ccc"], ["ccc"]]
                                    (sort (powerset ["a", "bb", "ccc"]))

tests :: TestTree
tests = $(testGroupGenerator)
