
{-# LANGUAGE TemplateHaskell #-}

module Parsing where

import Data.Char
import Data.Either
import Data.List

import Test.Tasty
import Test.Tasty.TH
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

case_parseCounterEmpty = assertEqual
                                    "Error occurred"
                                    (Left "Did not recognise \"Foo\" as a counter")
                                    (parseCounter [] "Foo")

case_parseCounterNoMatches = assertEqual
                                    "Error occurred"
                                    (Left "Did not recognise \"Foo\" as a counter")
                                    (parseCounter cs "Foo")
                                    where cs = [normalCounter "Bar", normalCounter "Baz"]

case_parseCounterAMatch = assertEqual
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

case_parseUnsureAboutCounterEmpty = assertEqual
                                            "Error occurred"
                                            (Left "Did not recognise \"Foo\" as a counter")
                                            (parseUnsureAboutCounter [] "Foo")

case_parseUnsureAboutCounterNoMatches = assertEqual
                                            "Error occurred"
                                            (Left "Did not recognise \"Foo\" as a counter")
                                            (parseUnsureAboutCounter cs "Foo")
                                            where cs = [normalCounter "Bar", normalCounter "Baz"]

case_parseUnsureAboutCounterDefinite = assertEqual
                                            "Error occurred"
                                            (Right (Definitely (normalCounter "Baz")))
                                            (parseUnsureAboutCounter cs "Baz")
                                            where cs = [normalCounter "Bar", normalCounter "Baz"]

case_parseUnsureAboutCounterPerhaps = assertEqual
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

case_separateOnSingle= assertEqual
                            "Error occurred"
                            ["foo", "bar", "baz"]
                            (separateOn "|" "foo|bar|baz")

case_separateOnMultiple= assertEqual
                            "Error occurred"
                            ["foo", "bar", "baz"]
                            (separateOn "|/" "foo/bar|baz")




strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

case_stripBlank = assertEqual
                    "Error occurred"
                    ""
                    (strip "")

case_stripNone = assertEqual
                    "Error occurred"
                    "Foo bar"
                    (strip "Foo bar")

case_stripBothSides = assertEqual
                    "Error occurred"
                    "foo  Bar"
                    (strip "    foo  Bar  ")




splitIntoWords :: String -> [String]
splitIntoWords input = map strip (separateOn ",&" (strip input))

case_splitIntoWordsEmpty = assertEqual
                                "Error occurred"
                                []
                                (splitIntoWords "")

case_splitIntoWordsWhitespace = assertEqual
                                "Error occurred"
                                []
                                (splitIntoWords "   ")

case_splitIntoWordsOne = assertEqual
                                "Error occurred"
                                ["Hello"]
                                (splitIntoWords "Hello")

case_splitIntoWordsOneWS = assertEqual
                                "Error occurred"
                                ["Hello"]
                                (splitIntoWords " Hello  ")

case_splitIntoWordsOneMulti = assertEqual
                                "Error occurred"
                                ["Hello There"]
                                (splitIntoWords "Hello There")

case_splitIntoWordsTwoClose = assertEqual
                                "Error occurred"
                                ["Hello", "There"]
                                (splitIntoWords "Hello,There")

case_splitIntoWordsTwoFar = assertEqual
                                "Error occurred"
                                ["Hello", "There"]
                                (splitIntoWords "Hello, There")

case_splitIntoWordsAmpersand = assertEqual
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

case_allRightEmpty = assertEqual
                        "Error occurred"
                        (Right [])
                        (allRight ([]::([Either String String])))

case_allRightAllGood = assertEqual
                        "Error occurred"
                        (Right [12, 18, 3])
                        (allRight [Right 12, Right 18, Right 3])

case_allRightOneBad = assertEqual
                        "Error occurred"
                        (Left "1 error(s): Wibble")
                        (allRight [Right 12, Left "Wibble", Right 3])





parseListOfCounter :: [Counter] -> String -> Either String [Counter]
parseListOfCounter cs s = allRight $ map (parseCounter cs) (splitIntoWords s)

case_parseListOfCounterBlankWithWhitespace = assertEqual
                                    "Error occurred"
                                    (Right [])
                                    (parseListOfCounter cs "    ")
                                        where cs = [normalCounter "Bar", normalCounter "Baz"]

case_parseListOfCounterNormal = assertEqual
                                    "Error occurred"
                                    (Right [normalCounter "Bar", normalCounter "Baz"])
                                    (parseListOfCounter cs "Bar, Baz")
                                        where cs = [normalCounter "Bar", normalCounter "Baz"]

case_parseListOfCounterNotUnsure = assertEqual
                                    "Error occurred"
                                    (Left "1 error(s): Counter \"Bar?\" cannot have a question mark")
                                    (parseListOfCounter cs "Bar?, Baz")
                                        where cs = [normalCounter "Bar", normalCounter "Baz"]





parseListOfUnsureAboutCounter :: [Counter] -> String -> Either String [(UnsureAbout Counter)]
parseListOfUnsureAboutCounter cs s = allRight $ map (parseUnsureAboutCounter cs) (splitIntoWords s)

case_parseListOfUnsureAboutCounterBlankWithWhitespace = assertEqual
                                        "Error occurred"
                                        (Right [])
                                        (parseListOfUnsureAboutCounter cs "       ")
                                          where cs = [foo, bar, baz, quux]
                                                foo = normalCounter "Foo"
                                                bar = emergencyCounter "Bar"
                                                baz = normalCounter "Baz"
                                                quux = normalCounter "Quux"

case_parseListOfUnsureAboutCounterNormal = assertEqual
                                        "Error occurred"
                                        (Right [Definitely bar, Perhaps baz, Definitely quux])
                                        (parseListOfUnsureAboutCounter cs "Bar, Baz? & Quux")
                                          where cs = [foo, bar, baz, quux]
                                                foo = normalCounter "Foo"
                                                bar = normalCounter "Bar"
                                                baz = emergencyCounter "Baz"
                                                quux = normalCounter "Quux"




ensureOnlyTwoParts :: [a] -> Either String ()
ensureOnlyTwoParts xs | (length xs) == 2 = Right ()
                      | otherwise = Left $ "You need 2 parts"




maybeRead x = case (reads x) of
                [(a, b)] -> Just a
                _ -> Nothing




readSpaces :: String -> Either String Integer
readSpaces text | text == "X" = Right 0
                | otherwise = case (maybeRead text) of
                       Nothing -> Left $ "Invalid number of slots " ++ text
                       Just spaces -> Right spaces




splitSpacesAndDate :: Integer -> String -> Either String (Integer, String)
splitSpacesAndDate defaultSpaces [] = Right (defaultSpaces, "")
splitSpacesAndDate defaultSpaces spacesAndDateWithWS =
    let spacesAndDate = strip spacesAndDateWithWS in
    if (head spacesAndDate) == '{'
    then
        let parts = separateOn "}" (tail spacesAndDate) in
        do  ensureOnlyTwoParts parts
            spaces <- readSpaces (parts !! 0)
            Right (spaces, (strip (parts !! 1)))
    else Right (defaultSpaces, spacesAndDate)

case_splitSpacesAndDateDefault = assertEqual
    "Error occurred"
    (Right (4, "8th Jan"))
    (splitSpacesAndDate 4 "  8th Jan ")

case_splitSpacesAndDateNoneNeeded = assertEqual
    "Error occurred"
    (Right (0, "8th Jan"))
    (splitSpacesAndDate 4 " {X}   8th Jan ")

case_splitSpacesAndDateUnusual = assertEqual
    "Error occurred"
    (Right (3, "8th Jan"))
    (splitSpacesAndDate 4 " {3}   8th Jan ")




tests :: TestTree
tests = $(testGroupGenerator)
