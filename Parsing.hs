
module Parsing where

import Data.Char
import Data.Either
import Data.List
import Test.Tasty.HUnit

import Planning



parseCounter :: [Counter] -> String -> Either String Counter
parseCounter [] name = Left $ "Did not recognise " ++ (show name) ++ " as a counter"
parseCounter (c:cs) name = let (Counter _ name') = c
                           in if (name == name')
                              then (Right c)
                              else if (name == (name' ++ "?"))
                                   then Left $ "Counter " ++ (show name) ++ " cannot have a question mark"
                                   else (parseCounter cs name)

test_parseCounterEmpty = testCase "parseCounter Empty" $ assertEqual
                                    "Error occurred"
                                    (Left "Did not recognise \"Foo\" as a counter")
                                    (parseCounter [] "Foo")

test_parseCounterNoMatches = testCase "parseCounter No Matches" $ assertEqual
                                    "Error occurred"
                                    (Left "Did not recognise \"Foo\" as a counter")
                                    (parseCounter cs "Foo")
                                    where cs = [normalCounter "Bar", normalCounter "Baz"]

test_parseCounterAMatch = testCase "parseCounter A Match" $ assertEqual
                                    "Error occurred"
                                    (Right (normalCounter "Bar"))
                                    (parseCounter cs "Bar")
                                    where cs = [normalCounter "Bar", normalCounter "Baz"]





parseUnsureAboutCounter :: [Counter] -> String -> Either String (UnsureAbout Counter)
parseUnsureAboutCounter [] name = Left $ "Did not recognise " ++ (show name) ++ " as a counter"
parseUnsureAboutCounter (c:cs) name = let (Counter _ name') = c
                                      in if (name == name')
                                         then (Right (Definitely c))
                                         else if (name == (name' ++ "?"))
                                              then (Right (Perhaps c))
                                              else (parseUnsureAboutCounter cs name)

test_parseUnsureAboutCounterEmpty = testCase "parseUnsureAboutCounter Empty" $ assertEqual
                                            "Error occurred"
                                            (Left "Did not recognise \"Foo\" as a counter")
                                            (parseUnsureAboutCounter [] "Foo")

test_parseUnsureAboutCounterNoMatches = testCase "parseUnsureAboutCounter No Matches" $ assertEqual
                                            "Error occurred"
                                            (Left "Did not recognise \"Foo\" as a counter")
                                            (parseUnsureAboutCounter cs "Foo")
                                            where cs = [normalCounter "Bar", normalCounter "Baz"]

test_parseUnsureAboutCounterDefinite = testCase "parseUnsureAboutCounter Definite" $ assertEqual
                                            "Error occurred"
                                            (Right (Definitely (normalCounter "Baz")))
                                            (parseUnsureAboutCounter cs "Baz")
                                            where cs = [normalCounter "Bar", normalCounter "Baz"]

test_parseUnsureAboutCounterPerhaps = testCase "parseUnsureAboutCounter Perhaps" $ assertEqual
                                            "Error occurred"
                                            (Right (Perhaps (normalCounter "Baz")))
                                            (parseUnsureAboutCounter cs "Baz?")
                                            where cs = [normalCounter "Bar", normalCounter "Baz"]




separateOn :: Eq a => [a] -> [a] -> [[a]]
separateOn xs [] = []
separateOn xs (y:ys) | (y `elem` xs) = []:(separateOn xs ys)
                     | otherwise = case (separateOn xs ys) of
                                    (zs:zss) -> (y:zs):zss
                                    [] -> [[y]]

test_separateOnSingle= testCase "separateOn Single" $ assertEqual
                            "Error occurred"
                            ["foo", "bar", "baz"]
                            (separateOn "|" "foo|bar|baz")

test_separateOnMultiple= testCase "separateOn Multiple" $ assertEqual
                            "Error occurred"
                            ["foo", "bar", "baz"]
                            (separateOn "|/" "foo/bar|baz")




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
splitIntoWords input = map strip (separateOn ",&" (strip input))

test_splitIntoWordsEmpty = testCase "splitIntoWords Empty" $ assertEqual
                                "Error occurred"
                                []
                                (splitIntoWords "")

test_splitIntoWordsWhitespace = testCase "splitIntoWords Whitespace" $ assertEqual
                                "Error occurred"
                                []
                                (splitIntoWords "   ")

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

test_splitIntoWordsAmpersand = testCase "splitIntoWords Ampersand" $ assertEqual
                                "Error occurred"
                                ["Hello", "There"]
                                (splitIntoWords "Hello & There")



joinWith :: String -> [String] -> String
joinWith sep [] = ""
joinWith sep (x:[]) = x
joinWith sep (x:xs) = x ++ sep ++ (joinWith sep xs)




allRight :: [Either String a] -> Either String [a]
allRight xs = case (lefts xs) of
                [] -> Right $ rights xs
                msgs -> Left $ (show (length msgs)) ++ " error(s): " ++ (joinWith ", " msgs)

test_allRightEmpty = testCase "allRight Empty" $ assertEqual
                        "Error occurred"
                        (Right [])
                        (allRight ([]::([Either String String])))

test_allRightAllGood = testCase "allRight All Good" $ assertEqual
                        "Error occurred"
                        (Right [12, 18, 3])
                        (allRight [Right 12, Right 18, Right 3])

test_allRightOneBad = testCase "allRight One Bad" $ assertEqual
                        "Error occurred"
                        (Left "1 error(s): Wibble")
                        (allRight [Right 12, Left "Wibble", Right 3])





parseListOfCounter :: [Counter] -> String -> Either String [Counter]
parseListOfCounter cs s = allRight $ map (parseCounter cs) (splitIntoWords s)

test_parseListOfCounterBlankWithWhitespace = testCase "parseListOfCounter Blank w/ whitespace" $ assertEqual
                                    "Error occurred"
                                    (Right [])
                                    (parseListOfCounter cs "    ")
                                        where cs = [normalCounter "Bar", normalCounter "Baz"]

test_parseListOfCounterNormal = testCase "parseListOfCounter Normal" $ assertEqual
                                    "Error occurred"
                                    (Right [normalCounter "Bar", normalCounter "Baz"])
                                    (parseListOfCounter cs "Bar, Baz")
                                        where cs = [normalCounter "Bar", normalCounter "Baz"]

test_parseListOfCounterNotUnsure = testCase "parseListOfCounter Not Unsure" $ assertEqual
                                    "Error occurred"
                                    (Left "1 error(s): Counter \"Bar?\" cannot have a question mark")
                                    (parseListOfCounter cs "Bar?, Baz")
                                        where cs = [normalCounter "Bar", normalCounter "Baz"]





parseListOfUnsureAboutCounter :: [Counter] -> String -> Either String [(UnsureAbout Counter)]
parseListOfUnsureAboutCounter cs s = allRight $ map (parseUnsureAboutCounter cs) (splitIntoWords s)

test_parseListOfUnsureAboutCounterBlankWithWhitespace = testCase "parseListOfUnsureAboutCounter Blank w/" $ assertEqual
                                        "Error occurred"
                                        (Right [])
                                        (parseListOfUnsureAboutCounter cs "       ")
                                          where cs = [foo, bar, baz, quux]
                                                foo = normalCounter "Foo"
                                                bar = emergencyCounter "Bar"
                                                baz = normalCounter "Baz"
                                                quux = normalCounter "Quux"

test_parseListOfUnsureAboutCounterNormal = testCase "parseListOfUnsureAboutCounter Normal" $ assertEqual
                                        "Error occurred"
                                        (Right [Definitely bar, Perhaps baz, Definitely quux])
                                        (parseListOfUnsureAboutCounter cs "Bar, Baz? & Quux")
                                          where cs = [foo, bar, baz, quux]
                                                foo = normalCounter "Foo"
                                                bar = normalCounter "Bar"
                                                baz = emergencyCounter "Baz"
                                                quux = normalCounter "Quux"



