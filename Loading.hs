
{-# LANGUAGE TemplateHaskell #-}

module Loading where

import Data.List.Split (splitOn)
import Data.List (foldl', isPrefixOf, isInfixOf)

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit

import Parsing (parseLine)
import Planning (Counter(..), normalCounter, emergencyCounter, Slot(..), allExcept)





data LoadedFile = LoadedFile
    { loadedCounters :: [Counter]
    , loadedSlots :: [Slot]
    , loadedDefaultSlotSize :: Integer
    , isLoading :: Bool
    , linesSeen :: Integer
    }
    deriving Show


newLoadedFile = LoadedFile
    { loadedCounters = []
    , loadedSlots = []
    , loadedDefaultSlotSize = 2  -- FIXME
    , isLoading = False
    , linesSeen = 0
    }


addSlot :: LoadedFile -> Slot -> LoadedFile
addSlot fileState newSlot =
    let appendedList = (loadedSlots fileState) ++ [newSlot]
    in fileState { loadedSlots = appendedList }



addCounter :: LoadedFile -> Counter -> LoadedFile
addCounter fileState newCounter =
    let appendedList = (loadedCounters fileState) ++ [newCounter]
    in fileState { loadedCounters = appendedList }


incremented :: LoadedFile -> LoadedFile
incremented fileState =
    let plusOne = (linesSeen fileState) + 1
    in fileState { linesSeen = plusOne }




digestLine :: LoadedFile -> String -> LoadedFile
digestLine fileState line = case (isLoading fileState) of
    True -> digestLineLoading (incremented fileState) line
    False -> digestLineIgnoring (incremented fileState) line

digestLineIgnoring :: LoadedFile -> String -> LoadedFile
digestLineIgnoring fileState line = case line of
    ">>>>> ON" -> fileState { isLoading = True }
    _ -> fileState

digestLineLoading :: LoadedFile -> String -> LoadedFile
digestLineLoading fileState line
    | line == "<<<<< OFF" = fileState { isLoading = False }
    | "set " `isPrefixOf` line =
            let [_set, key, value] = words line in
            case key of
                "defaultSlotSize" -> fileState { loadedDefaultSlotSize = (read value) }
                _ -> error $ "Unknown setting: " ++ key
    | "addCounterNormal " `isPrefixOf` line =
            let [_add, name] = words line in
            addCounter fileState (normalCounter name)
    | "addCounterEmergency " `isPrefixOf` line =
            let [_add, name] = words line in
            addCounter fileState (emergencyCounter name)
    | "===" `isPrefixOf` line = fileState
    | isHeader line = fileState
    | otherwise = case (parseLine (loadedDefaultSlotSize fileState) (loadedCounters fileState) line) of
        Left message -> error $ "Line " ++ (show (linesSeen fileState)) ++ ": " ++ message
        Right newSlot -> addSlot fileState newSlot

isHeader line = "Not Available" `isInfixOf` line

case_digestLine_defaultSlotSize = do
    let initial = newLoadedFile { isLoading = True }
    let line = "set defaultSlotSize 1234"
    1234 @=? loadedDefaultSlotSize (digestLine initial line)




loadFromPath :: String -> IO LoadedFile
loadFromPath path = do
    textFromFile <- readFile path
    let linesInFile = splitOn "\n" textFromFile
    let initialFileState = newLoadedFile
    return $ foldl' digestLine initialFileState linesInFile

case_loadExampleCounters = do
    loadedFile <- loadFromPath "example.rota"
    loadedCounters loadedFile @?= [
          Counter True  "Alice"
        , Counter True  "Bob"
        , Counter False "Carol"
        , Counter True  "Dave"
        , Counter True  "Eve"
        ]

case_loadExampleSlotsLength = do
    loadedFile <- loadFromPath "example.rota"
    7 @=? length (loadedSlots loadedFile)

case_loadExampleSlotFive = do
    loadedFile <- loadFromPath "example.rota"
    let alice = head (loadedCounters loadedFile)
    Slot 2 "5th Jan" (allExcept [alice]) @=? (loadedSlots loadedFile) !! 4




tests :: TestTree
tests = $(testGroupGenerator)
