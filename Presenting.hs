
module Presenting where

import Test.Tasty.HUnit

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
