
{-# LANGUAGE TemplateHaskell #-}

module Parsing where

import Data.Char
import Data.Either
import Data.List
import Data.List.Split

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

case_parseCounterEmpty = Left msg @=? parseCounter [] "Foo"
  where msg = "Did not recognise \"Foo\" as a counter"

case_parseCounterNoMatches = Left msg @=? parseCounter cs "Foo"
  where cs = [normalCounter "Bar", normalCounter "Baz"]
        msg = "Did not recognise \"Foo\" as a counter"

case_parseCounterAMatch = Right (normalCounter "Bar") @=? parseCounter cs "Bar"
  where cs = [normalCounter "Bar", normalCounter "Baz"]





parseUnsureAboutCounter :: [Counter] -> String -> Either String (UnsureAbout Counter)
parseUnsureAboutCounter [] name = Left $ "Did not recognise " ++ (show name) ++ " as a counter"
parseUnsureAboutCounter (c:cs) name = let (Counter _ name') = c
                                      in if (name == name')
                                         then (Right (Definitely c))
                                         else if (name == (name' ++ "?"))
                                              then (Right (Perhaps c))
                                              else (parseUnsureAboutCounter cs name)

case_parseUnsureAboutCounterEmpty = Left msg @=? parseUnsureAboutCounter [] "Foo"
  where msg = "Did not recognise \"Foo\" as a counter"

case_parseUnsureAboutCounterNoMatches = Left msg @=? parseUnsureAboutCounter cs "Foo"
  where cs = [normalCounter "Bar", normalCounter "Baz"]
        msg = "Did not recognise \"Foo\" as a counter"

case_parseUnsureAboutCounterDefinite = Right (Definitely (normalCounter "Baz")) @=? parseUnsureAboutCounter cs "Baz"
  where cs = [normalCounter "Bar", normalCounter "Baz"]

case_parseUnsureAboutCounterPerhaps = (Right (Perhaps (normalCounter "Baz"))) @=? parseUnsureAboutCounter cs "Baz?"
  where cs = [normalCounter "Bar", normalCounter "Baz"]




separateOn :: Eq a => [a] -> [a] -> [[a]]
separateOn xs [] = []
separateOn xs (y:ys) | (y `elem` xs) = []:(separateOn xs ys)
                     | otherwise = case (separateOn xs ys) of
                                    (zs:zss) -> (y:zs):zss
                                    [] -> [[y]]

case_separateOnSingle= ["foo", "bar", "baz"] @=? separateOn "|" "foo|bar|baz"

case_separateOnMultiple= ["foo", "bar", "baz"] @=? separateOn "|/" "foo/bar|baz"




strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

case_stripBlank = "" @=? strip ""

case_stripNone = "Foo bar" @=? strip "Foo bar"

case_stripBothSides = "foo  Bar" @=? strip "    foo  Bar  "




splitIntoWords :: String -> [String]
splitIntoWords input = map strip (separateOn ",&" (strip input))

case_splitIntoWordsEmpty = [] @=? splitIntoWords ""

case_splitIntoWordsWhitespace = [] @=? splitIntoWords "   "

case_splitIntoWordsOne = ["Hello"] @=? splitIntoWords "Hello"

case_splitIntoWordsOneWS = ["Hello"] @=? splitIntoWords " Hello  "

case_splitIntoWordsOneMulti = ["Hello There"] @=? splitIntoWords "Hello There"

case_splitIntoWordsTwoClose = ["Hello", "There"] @=? splitIntoWords "Hello,There"

case_splitIntoWordsTwoFar = ["Hello", "There"] @=? splitIntoWords "Hello, There"

case_splitIntoWordsAmpersand = ["Hello", "There"] @=? splitIntoWords "Hello & There"



joinWith :: String -> [String] -> String
joinWith sep [] = ""
joinWith sep (x:[]) = x
joinWith sep (x:xs) = x ++ sep ++ (joinWith sep xs)




allRight :: [Either String a] -> Either String [a]
allRight xs = case (lefts xs) of
                [] -> Right $ rights xs
                msgs -> Left $ (show (length msgs)) ++ " error(s): " ++ (joinWith ", " msgs)

case_allRightEmpty = Right [] @=?  (allRight ([]::([Either String String])))

case_allRightAllGood = Right [12, 18, 3] @=? allRight [Right 12, Right 18, Right 3]

case_allRightOneBad = Left "1 error(s): Wibble" @=? allRight [Right 12, Left "Wibble", Right 3]





parseListOfCounter :: [Counter] -> String -> Either String [Counter]
parseListOfCounter cs s = allRight $ map (parseCounter cs) (splitIntoWords s)

case_parseListOfCounterBlankWithWhitespace = Right [] @=? parseListOfCounter cs "    "
  where cs = [normalCounter "Bar", normalCounter "Baz"]

case_parseListOfCounterNormal = Right cs @=? parseListOfCounter cs "Bar, Baz"
  where cs = [normalCounter "Bar", normalCounter "Baz"]

case_parseListOfCounterNotUnsure = Left msg @=? parseListOfCounter cs "Bar?, Baz"
  where cs = [normalCounter "Bar", normalCounter "Baz"]
        msg = "1 error(s): Counter \"Bar?\" cannot have a question mark"





parseListOfUnsureAboutCounter :: [Counter] -> String -> Either String [(UnsureAbout Counter)]
parseListOfUnsureAboutCounter cs s = allRight $ map (parseUnsureAboutCounter cs) (splitIntoWords s)

case_parseListOfUnsureAboutCounterBlankWithWhitespace = Right [] @=? parseListOfUnsureAboutCounter cs "       "
                                          where cs = [foo, bar, baz, quux]
                                                foo = normalCounter "Foo"
                                                bar = emergencyCounter "Bar"
                                                baz = normalCounter "Baz"
                                                quux = normalCounter "Quux"

case_parseListOfUnsureAboutCounterNormal = Right [Definitely bar, Perhaps baz, Definitely quux] @=? parseListOfUnsureAboutCounter cs "Bar, Baz? & Quux"
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

case_splitSpacesAndDateDefault = Right (4, "8th Jan") @=? splitSpacesAndDate 4 "  8th Jan "

case_splitSpacesAndDateNoneNeeded = Right (0, "8th Jan") @=? splitSpacesAndDate 4 " {X}   8th Jan "

case_splitSpacesAndDateUnusual = Right (3, "8th Jan") @=? splitSpacesAndDate 4 " {3}   8th Jan "






parseLine :: Integer -> [Counter] -> String -> Either String Slot
parseLine defaultSpaces counters line = case (splitOn "|" line) of
    [_, spacesAndDate, yes, no, _] -> do
        yes_prefs <- parseListOfCounter counters yes
        no_prefs <- parseListOfUnsureAboutCounter counters no
        let prefs = yes_prefs `without` no_prefs
        (spaces, date) <- splitSpacesAndDate defaultSpaces spacesAndDate
        Right $ Slot spaces (strip date) prefs
    _ -> Left ("Invalid columns: '" ++ line ++ "'")

case_parseLineGoodBare = Right (Slot 2 "1st Jan" allFree)
                         @=? parseLine 2 [] " |  1st Jan |  |  | "

case_parseLineGoodComplicated = Right (Slot 3 "2nd Jan" (allExcept [two]))
                                @=? parseLine 2 cs "| {3} 2nd Jan |  | Two|"
                                where   one = normalCounter "One"
                                        two = normalCounter "Two"
                                        three = normalCounter "Three"
                                        four = normalCounter "Four"
                                        cs = [one, two, three, four]

case_parseLineSillyColumns = Left "Invalid columns: '||||||||||'"
                             @=? parseLine 2 [] "||||||||||"

case_parseLineInvalidName = Left "1 error(s): Did not recognise \"Mickey\" as a counter"
                            @=? parseLine 2 cs "| 4th Jan | Mickey |  |"
                            where cs = [normalCounter "Alfa", normalCounter "Beeta"]




tests :: TestTree
tests = $(testGroupGenerator)
