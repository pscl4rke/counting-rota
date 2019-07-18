
{-# LANGUAGE TemplateHaskell #-}

module Loading where

import Data.List.Split (splitOn)
import Data.List (foldl')

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit

import Parsing (parseLine)
import Planning (Counter, Slot(..), allExcept)
import Requirements (hardcodedCounters)





data LoadedFile = LoadedFile
    { loadedCounters :: [Counter]
    , loadedSlots :: [Slot]
    , isLoading :: Bool
    }
    deriving Show


newLoadedFile = LoadedFile
    { loadedCounters = []
    , loadedSlots = []
    , isLoading = False
    }


addSlot :: LoadedFile -> Slot -> LoadedFile
addSlot fileState newSlot =
    let appendedList = (loadedSlots fileState) ++ [newSlot]
    in fileState { loadedSlots = appendedList }




digestLine :: LoadedFile -> String -> LoadedFile
digestLine fileState line = case (isLoading fileState) of
    True -> digestLineLoading fileState line
    False -> digestLineIgnoring fileState line

digestLineIgnoring :: LoadedFile -> String -> LoadedFile
digestLineIgnoring fileState line = case line of
    ">>>>> ON" -> fileState { isLoading = True }
    _ -> fileState

digestLineLoading :: LoadedFile -> String -> LoadedFile
digestLineLoading fileState line
    | line == "<<<<< OFF" = fileState { isLoading = False }
    | otherwise = case (parseLine (loadedCounters fileState) line) of
        Left message -> error message
        Right newSlot -> addSlot fileState newSlot




loadFromPath :: String -> IO LoadedFile
loadFromPath path = do
    textFromFile <- readFile path
    let linesInFile = splitOn "\n" textFromFile
    let initialFileState = newLoadedFile { loadedCounters = hardcodedCounters }
    return $ foldl' digestLine initialFileState linesInFile

case_loadExampleCounters = do
    loadedFile <- loadFromPath "example.rota"
    hardcodedCounters @=? loadedCounters loadedFile

case_loadExampleSlotsLength = do
    loadedFile <- loadFromPath "example.rota"
    7 @=? length (loadedSlots loadedFile)

case_loadExampleSlotFive = do
    loadedFile <- loadFromPath "example.rota"
    let alice = head (loadedCounters loadedFile)
    Slot 2 "5th Jan" (allExcept [alice]) @=? (loadedSlots loadedFile) !! 4




tests :: TestTree
tests = $(testGroupGenerator)
