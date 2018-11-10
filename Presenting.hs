
{-# LANGUAGE TemplateHaskell #-}

module Presenting where

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit





padLeft :: Int -> String -> String
padLeft n [] = replicate n ' '
padLeft n (c:cs) = c:(padLeft (n - 1) cs)

case_padLeftNormal = "Foo Bar   " @=? padLeft 10 "Foo Bar"

case_padLeftLong = "Foo Bar Baz Quux" @=? padLeft 10 "Foo Bar Baz Quux"





padRight :: Int -> String -> String
padRight n cs = (replicate (n - (length cs)) ' ') ++ cs

case_padRightNormal = "   Foo Bar" @=? padRight 10 "Foo Bar"

case_padRightLong = "Foo Bar Baz Quux" @=? padRight 10 "Foo Bar Baz Quux"






showStrings :: [String] -> String
showStrings []            = ""
showStrings (x:[])        = x
showStrings (x:y:[])      = x ++ " & " ++ y
showStrings (x:xs)        = x ++ ", " ++ (showStrings xs)

case_showStringsNone = "" @=? showStrings []

case_showStringsOne = "Foo" @=? showStrings ["Foo"]

case_showStringsTwo = "Foo & Bar" @=? showStrings ["Foo", "Bar"]

case_showStringsThree = "Foo, Bar & Baz" @=? showStrings ["Foo", "Bar", "Baz"]



tests :: TestTree
tests = $(testGroupGenerator)
