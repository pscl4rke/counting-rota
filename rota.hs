
import Control.Monad (forM_)
import Data.List (sortBy)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (comparing)
import System.Environment (getArgs)

import Dating
import Planning
import Requirements
import Scoring

import Test.HUnit




padLeft :: Int -> String -> String
padLeft n [] = replicate n ' '
padLeft n (c:cs) = c:(padLeft (n - 1) cs)

-- annoyingly import errors stop me from including this
test_padLeftNormal = TestCase $ assertEqual "PadLeft Normal"
                                    "Foo Bar   "
                                    (padLeft 10 "Foo Bar")

padRight :: Int -> String -> String
padRight n cs = (replicate (n - (length cs)) ' ') ++ cs





n (Counter name) = name
showNames :: [Counter] -> String
showNames []            = ""
showNames (x:[])        = (n x)
showNames (x:y:[])      = (n x) ++ " & " ++ (n y)
showNames (x:xs)        = (n x) ++ ", " ++ (showNames xs)





displayScorecard :: Scorecard -> IO ()
displayScorecard card = do
    putStrLn $ "Two weeks in a row:" ++ show (scoreTwoWeeksInARow card)
    putStrLn $ "Above and beyond:" ++ show (scoreAboveAndBeyond card)
    putStrLn $ "Partner variety:" ++ show (scorePartnerVariety card)
    putStrLn $ "TOTAL:" ++ show (scoreTotal card)





display :: [Counter] -> Rota -> IO ()
display allCounters rota = do
    putStrLn (replicate 72 '=')
    putStrLn "|               | Counting                  | Not Available            |"
    putStrLn (replicate 72 '=')
    let (Rota slotmap) = rota
    forM_ slotmap $ \(slot, theseCounters) -> do
        let Slot size description prefs = slot
        putStrLn $ "| "
                    ++ (padRight 12 description)
                    ++ "  | "
                    ++ (padLeft 26 (showNames theseCounters))
                    ++ "| "
                    ++ (padLeft 25 (showNames (available prefs counters)))
                    ++ "|"
    putStrLn (replicate 72 '=')
  where available prefs counters = filter (not . (canDo prefs)) counters

-- BACKPORTED
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

main :: IO ()
main = do
    args <- getArgs
    let command = fromMaybe "help" $ listToMaybe args
    case command of
        "rota" -> do
            let rotas = sortOn (overallStrikes counters) $ usableRotas counters slots
            forM_ (take 5 rotas) $ \rota -> do
                putStrLn ""
                displayScorecard $ scorecard counters rota
                display counters rota
        "blank" -> do
            sundays <- nextFifteenSundays
            display counters (blankRota 2 sundays)
        _ -> do
            putStrLn "Want to write help here"
