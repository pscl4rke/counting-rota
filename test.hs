
--module Test where

import System.Exit
import Test.HUnit

import qualified Planning
import qualified Scoring
import qualified Set

allTests :: [Test]
allTests =
    [ Set.test_powersetEmpty
    , Set.test_powersetOne
    , Set.test_powersetThree
    , Planning.test_canDoGoodPrefs
    , Planning.test_canDoBadPrefs
    , Planning.test_acceptableMatch
    , Planning.test_acceptableUnder
    , Planning.test_acceptableOver
    , Planning.test_acceptableGoodPrefs
    , Planning.test_acceptableBadPrefs
    , Planning.test_noUsableRotas
    , Planning.test_usableRotas
    , Scoring.test_pairOffEmpty
    , Scoring.test_pairOffSingle
    , Scoring.test_pairOffDouble
    , Scoring.test_pairOffMany
    ]

main = do
    counts <- runTestTT $ TestList allTests
    let wentBad = ((errors counts) + (failures counts)) > 0
    if wentBad then exitFailure else exitSuccess
