
module Parsing where

import Test.Tasty.HUnit

import Planning



parseCounter :: [Counter] -> String -> Maybe Counter
parseCounter [] _ = Nothing
parseCounter (c:cs) name = let (Counter name') = c
                           in if (name == name')
                              then (Just c)
                              else (parseCounter cs name)

test_parseCounterEmpty = testCase "parseCounter Empty" $ assertEqual
                                    "Error occurred"
                                    Nothing
                                    (parseCounter [] "Foo")

test_parseCounterNoMatches = testCase "parseCounter No Matches" $ assertEqual
                                    "Error occurred"
                                    Nothing
                                    (parseCounter cs "Foo")
                                    where cs = [Counter "Bar", Counter "Baz"]

test_parseCounterAMatch = testCase "parseCounter A Match" $ assertEqual
                                    "Error occurred"
                                    (Just (Counter "Bar"))
                                    (parseCounter cs "Bar")
                                    where cs = [Counter "Bar", Counter "Baz"]
