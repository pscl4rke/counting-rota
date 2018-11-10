
{-# LANGUAGE TemplateHaskell #-}

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit

import qualified Set
import qualified Planning
import qualified Scoring
import qualified Presenting
import qualified Parsing

-- GENERAL TESTING NOTES!
--  The API: https://github.com/feuerbach/tasty/blob/master/hunit/Test/Tasty/HUnit/Orig.hs
--  funcReturningBool arg1 arg2etc @? "Message if fails"
--  expectedValue @=? funcReturningActualValue arg1 arg2etc
--  funcReturningActualValue arg1 arg2etc @?= expectedValue

allTests =
    [ Set.tests
    , Planning.tests
    , Scoring.tests
    , Presenting.tests
    , Parsing.tests
    ]

main = defaultMain $ testGroup "counting-rota" allTests
