
{-# LANGUAGE TemplateHaskell #-}

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit

import qualified Set
import qualified Planning
import qualified Scoring
import qualified Presenting
import qualified Parsing

allTests =
    [ Set.tests
    , Planning.tests
    , Scoring.tests
    , Presenting.tests
    , Parsing.tests
    ]

main = defaultMain $ testGroup "counting-rota" allTests
