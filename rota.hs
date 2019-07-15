
import Control.Monad (forM_)
import Data.List (sortBy)
import Data.Ord (comparing)
import System.Environment (getArgs)

import Dating
import Planning
import Presenting
import Requirements
import Scoring





showNames :: [Counter] -> String
showNames counters = showStrings $ map nameOf counters
  where nameOf (Counter _ name) = name




displayScorecard :: Scorecard -> IO ()
displayScorecard card = do
    putStrLn $ "Trying to avoid:" ++ show (scoreTryingToAvoid card)
    putStrLn $ "Two weeks in a row:" ++ show (scoreTwoWeeksInARow card)
    putStrLn $ "Above and beyond:" ++ show (scoreAboveAndBeyond card)
    putStrLn $ "Partner variety:" ++ show (scorePartnerVariety card)
    putStrLn $ "TOTAL:" ++ show (scoreTotal card)




showNotAvailable :: Preferences -> String
showNotAvailable prefs = showStrings (map showSomething (notAvailable prefs))
  where notAvailable (Preferences abc) = filter worthShowing abc
        worthShowing (counter, availability) = case availability of
            CannotDo -> True
            WantsToAvoid -> True
            _ -> False
        showSomething (counter, availability) = case availability of
            CannotDo -> nameOf counter
            WantsToAvoid -> (nameOf counter) ++ "?"
            _ -> "ERROR!"
        nameOf (Counter _ x) = x





displaySize :: Integer -> Integer -> String
displaySize defaultSize size | size == defaultSize = "   "
                             | size == 0 = "{X}"
                             | otherwise = "{" ++ (show size) ++ "}"





display :: [Counter] -> Rota -> IO ()
display allCounters rota = do
    putStrLn (replicate 72 '=')
    putStrLn "|               | Counting                | Not Available              |"
    putStrLn (replicate 72 '=')
    let (Rota slotmap) = rota
    forM_ slotmap $ \(slot, theseCounters) -> do
        let Slot size description prefs = slot
        let defaultSize = 2 -- FIXME duplicating Requirements.hs
        putStrLn $ "| "
                    ++ (displaySize defaultSize size)
                    ++ (padRight 10 description)
                    ++ " | "
                    ++ (padLeft 24 (showNames theseCounters))
                    ++ "| "
                    ++ (padLeft 27 (showNotAvailable prefs))
                    ++ "|"
    putStrLn (replicate 72 '=')





-- BACKPORTED
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))





main :: IO ()
main = do
    args <- getArgs
    let command = if (null args) then ["help"] else args
    case command of
        ["rota", filePath] -> do
            (counters, slots) <- loadCountersAndSlotsFromPath filePath
            let rotas = sortOn (overallStrikes counters) $ usableRotas counters slots
            case (length rotas) of
                0 -> putStrLn "There are no rotas that could be generated"
                _ -> forM_ (take 5 rotas) $ \rota -> do
                    putStrLn ""
                    displayScorecard $ scorecard counters rota
                    display counters rota
        ["blank"] -> do
            sundays <- nextFifteenSundays
            display [] (blankRota 2 sundays)
        _ -> do
            putStrLn "Usage:"
            putStrLn "  rota blank          Generate a blank rota"
            putStrLn "  rota rota <file>    Suggest satisfactory rotas"
