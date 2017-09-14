
import Test.Tasty

import qualified Set
import qualified Planning
import qualified Scoring
import qualified Presenting
import qualified Parsing

setTests =
    [ Set.test_powersetEmpty
    , Set.test_powersetOne
    , Set.test_powersetThree
    ]

planningTests =
    [ Planning.test_canDoGoodPrefs
    , Planning.test_canDoBadPrefs
    , Planning.test_acceptableMatch
    , Planning.test_acceptableUnder
    , Planning.test_acceptableOver
    , Planning.test_acceptableGoodPrefs
    , Planning.test_acceptableBadPrefs
    , Planning.test_noUsableRotas
    , Planning.test_usableRotas
    ]

scoringTests =
    [ Scoring.test_pairOffEmpty
    , Scoring.test_pairOffSingle
    , Scoring.test_pairOffDouble
    , Scoring.test_pairOffMany
    ]

presentingTests =
    [ Presenting.test_padLeftNormal
    , Presenting.test_padLeftLong
    , Presenting.test_padRightNormal
    , Presenting.test_padRightLong
    , Presenting.test_showStringsNone
    , Presenting.test_showStringsOne
    , Presenting.test_showStringsTwo
    , Presenting.test_showStringsThree
    ]

parsingTests =
    [ Parsing.test_parseCounterEmpty
    , Parsing.test_parseCounterNoMatches
    , Parsing.test_parseCounterAMatch
    , Parsing.test_parseUnsureAboutCounterEmpty
    , Parsing.test_parseUnsureAboutCounterNoMatches
    , Parsing.test_parseUnsureAboutCounterDefinite
    , Parsing.test_parseUnsureAboutCounterPerhaps
    ]

allTests =
    [ testGroup "Set.hs" setTests
    , testGroup "Planning.hs" planningTests
    , testGroup "Scoring.hs" scoringTests
    , testGroup "Presenting.hs" presentingTests
    , testGroup "Parsing.hs" parsingTests
    ]

main = defaultMain $ testGroup "counting-rota" allTests
