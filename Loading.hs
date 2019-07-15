
{-# LANGUAGE TemplateHaskell #-}

module Loading where

import Data.List.Split (splitOn)

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit

import Planning (Counter, Slot)
import Requirements (hardcodedCounters, loadSlotsFromLines)





data LoadedFile = LoadedFile
    { loadedCounters :: [Counter]
    , loadedSlots :: [Slot]
    }




loadFromPath :: String -> IO LoadedFile
loadFromPath path = do
    textFromFile <- readFile path
    let loadedSlots = loadSlotsFromLines (splitOn "\n" textFromFile)
    return LoadedFile
        { loadedCounters = hardcodedCounters
        , loadedSlots = loadedSlots
        }

case_loadExampleCounters = do
    loadedFile <- loadFromPath "example.rota"
    hardcodedCounters @=? loadedCounters loadedFile

case_loadExampleSlotsLength = do
    loadedFile <- loadFromPath "example.rota"
    7 @=? length (loadedSlots loadedFile)




tests :: TestTree
tests = $(testGroupGenerator)
