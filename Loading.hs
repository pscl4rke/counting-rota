
{-# LANGUAGE TemplateHaskell #-}

module Loading where

import Data.List.Split (splitOn)

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit

import Planning (Counter, Slot)
import Requirements (hardcodedCounters, loadSlotsFromLines)





loadCountersAndSlotsFromPath :: String -> IO ([Counter], [Slot])
loadCountersAndSlotsFromPath path = do
    textFromFile <- readFile path
    let loadedSlots = loadSlotsFromLines (splitOn "\n" textFromFile)
    return (hardcodedCounters, loadedSlots)

case_loadExampleCounters = do
    (counters, slots) <- loadCountersAndSlotsFromPath "example.rota"
    hardcodedCounters @=? counters

case_loadExampleSlotsLength = do
    (counters, slots) <- loadCountersAndSlotsFromPath "example.rota"
    7 @=? length slots




tests :: TestTree
tests = $(testGroupGenerator)
