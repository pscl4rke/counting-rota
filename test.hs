
--module Test where

import System.Exit
import Test.HUnit

import qualified Planning
import qualified Set

allTests :: [Test]
allTests =
    [ Set.test_powersetEmpty
    , Set.test_powersetOne
    , Set.test_powersetThree
    , Planning.test_allRotas
    , Planning.test_acceptableMatch
    , Planning.test_acceptableUnder
    , Planning.test_acceptableOver
    , Planning.test_usableRotas
    ]

main = do
    counts <- runTestTT $ TestList allTests
    let wentBad = ((errors counts) + (failures counts)) > 0
    if wentBad then exitFailure else exitSuccess
