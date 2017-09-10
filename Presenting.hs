
module Presenting where

import Test.Tasty.HUnit





padLeft :: Int -> String -> String
padLeft n [] = replicate n ' '
padLeft n (c:cs) = c:(padLeft (n - 1) cs)

test_padLeftNormal = testCase "padLeft Normal" $ assertEqual
                                    "Error occurred"
                                    "Foo Bar   "
                                    (padLeft 10 "Foo Bar")

test_padLeftLong = testCase "padLeft Long" $ assertEqual
                                    "Error occurred"
                                    "Foo Bar Baz Quux"
                                    (padLeft 10 "Foo Bar Baz Quux")





padRight :: Int -> String -> String
padRight n cs = (replicate (n - (length cs)) ' ') ++ cs

test_padRightNormal = testCase "padRight Normal" $ assertEqual
                                    "Error occurred"
                                    "   Foo Bar"
                                    (padRight 10 "Foo Bar")

test_padRightLong = testCase "padRight Long" $ assertEqual
                                    "Error occurred"
                                    "Foo Bar Baz Quux"
                                    (padRight 10 "Foo Bar Baz Quux")






showStrings :: [String] -> String
showStrings []            = ""
showStrings (x:[])        = x
showStrings (x:y:[])      = x ++ " & " ++ y
showStrings (x:xs)        = x ++ ", " ++ (showStrings xs)

test_showStringsNone = testCase "showStrings None" $ assertEqual
                                    "Error occurred"
                                    ""
                                    (showStrings [])

test_showStringsOne = testCase "showStrings One" $ assertEqual
                                    "Error occurred"
                                    "Foo"
                                    (showStrings ["Foo"])

test_showStringsTwo = testCase "showStrings Two" $ assertEqual
                                    "Error occurred"
                                    "Foo & Bar"
                                    (showStrings ["Foo", "Bar"])

test_showStringsThree = testCase "showStrings Three" $ assertEqual
                                    "Error occurred"
                                    "Foo, Bar & Baz"
                                    (showStrings ["Foo", "Bar", "Baz"])
