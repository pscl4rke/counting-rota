
module Parsing where

import Data.Char
import Data.List
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





parseUnsureAboutCounter :: [Counter] -> String -> Maybe (UnsureAbout Counter)
parseUnsureAboutCounter [] _ = Nothing
parseUnsureAboutCounter (c:cs) name = let (Counter name') = c
                                      in if (name == name')
                                         then (Just (Definitely c))
                                         else if (name == (name' ++ "?"))
                                              then (Just (Perhaps c))
                                              else (parseUnsureAboutCounter cs name)

test_parseUnsureAboutCounterEmpty = testCase "parseUnsureAboutCounter Empty" $ assertEqual
                                            "Error occurred"
                                            Nothing
                                            (parseUnsureAboutCounter [] "Foo")

test_parseUnsureAboutCounterNoMatches = testCase "parseUnsureAboutCounter No Matches" $ assertEqual
                                            "Error occurred"
                                            Nothing
                                            (parseUnsureAboutCounter cs "Foo")
                                            where cs = [Counter "Bar", Counter "Baz"]

test_parseUnsureAboutCounterDefinite = testCase "parseUnsureAboutCounter Definite" $ assertEqual
                                            "Error occurred"
                                            (Just (Definitely (Counter "Baz")))
                                            (parseUnsureAboutCounter cs "Baz")
                                            where cs = [Counter "Bar", Counter "Baz"]

test_parseUnsureAboutCounterPerhaps = testCase "parseUnsureAboutCounter Perhaps" $ assertEqual
                                            "Error occurred"
                                            (Just (Perhaps (Counter "Baz")))
                                            (parseUnsureAboutCounter cs "Baz?")
                                            where cs = [Counter "Bar", Counter "Baz"]




separateOn :: Eq a => a -> [a] -> [[a]]
separateOn x [] = []
separateOn x (y:ys) | x == y = []:(separateOn x ys)
                    | otherwise = case (separateOn x ys) of
                                    (zs:zss) -> (y:zs):zss
                                    [] -> [[y]]

test_separateOnMultiple= testCase "separateOn Multiple" $ assertEqual
                            "Error occurred"
                            ["foo", "bar", "baz"]
                            (separateOn '|' "foo|bar|baz")




strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

test_stripBlank = testCase "strip Blank" $ assertEqual
                    "Error occurred"
                    ""
                    (strip "")

test_stripNone = testCase "strip None" $ assertEqual
                    "Error occurred"
                    "Foo bar"
                    (strip "Foo bar")

test_stripBothSides = testCase "strip BothSides" $ assertEqual
                    "Error occurred"
                    "foo  Bar"
                    (strip "    foo  Bar  ")




splitIntoWords :: String -> [String]
splitIntoWords input = map strip (separateOn ',' input)

test_splitIntoWordsEmpty = testCase "splitIntoWords Empty" $ assertEqual
                                "Error occurred"
                                []
                                (splitIntoWords "")

test_splitIntoWordsOne = testCase "splitIntoWords One" $ assertEqual
                                "Error occurred"
                                ["Hello"]
                                (splitIntoWords "Hello")

test_splitIntoWordsOneWS = testCase "splitIntoWords One w/ whitespace" $ assertEqual
                                "Error occurred"
                                ["Hello"]
                                (splitIntoWords " Hello  ")

test_splitIntoWordsOneMulti = testCase "splitIntoWords OneMulti" $ assertEqual
                                "Error occurred"
                                ["Hello There"]
                                (splitIntoWords "Hello There")

test_splitIntoWordsTwoClose = testCase "splitIntoWords TwoClose" $ assertEqual
                                "Error occurred"
                                ["Hello", "There"]
                                (splitIntoWords "Hello,There")

test_splitIntoWordsTwoFar = testCase "splitIntoWords TwoFar" $ assertEqual
                                "Error occurred"
                                ["Hello", "There"]
                                (splitIntoWords "Hello, There")




allJust :: [Maybe a] -> Maybe [a]
allJust [] = Just []
allJust (Nothing:_) = Nothing
allJust ((Just x):xs) = case (allJust xs) of
                          Nothing -> Nothing
                          Just ys -> Just (x:ys)

test_allJustEmpty = testCase "allJust Empty" $ assertEqual
                        "Error occurred"
                        (Just [])
                        (allJust ([]::([Maybe String])))

test_allJustAllGood = testCase "allJust All Good" $ assertEqual
                        "Error occurred"
                        (Just [12, 18, 3])
                        (allJust [Just 12, Just 18, Just 3])

test_allJustOneBad = testCase "allJust One Bad" $ assertEqual
                        "Error occurred"
                        Nothing
                        (allJust [Just 12, Nothing, Just 3])





parseListOfCounter :: [Counter] -> String -> Maybe [Counter]
parseListOfCounter cs s = allJust $ map (parseCounter cs) (splitIntoWords s)

test_parseListOfCounterNormal = testCase "parseListOfCounter Normal" $ assertEqual
                                    "Error occurred"
                                    (Just [Counter "Bar", Counter "Baz"])
                                    (parseListOfCounter cs "Bar, Baz")
                                        where cs = [Counter "Bar", Counter "Baz"]

test_parseListOfCounterNotUnsure = testCase "parseListOfCounter Not Unsure" $ assertEqual
                                    "Error occurred"
                                    Nothing
                                    (parseListOfCounter cs "Bar?, Baz")
                                        where cs = [Counter "Bar", Counter "Baz"]





parseListOfUnsureAboutCounter :: [Counter] -> String -> Maybe [(UnsureAbout Counter)]
parseListOfUnsureAboutCounter cs s = allJust $ map (parseUnsureAboutCounter cs) (splitIntoWords s)

test_parseListOfUnsureAboutCounterNormal = testCase "parseListOfUnsureAboutCounter Normal" $ assertEqual
                                        "Error occurred"
                                        (Just [Definitely (Counter "Bar"), Perhaps (Counter "Baz")])
                                        (parseListOfUnsureAboutCounter cs "Bar, Baz?")
                                            where cs = [Counter "Bar", Counter "Baz"]



