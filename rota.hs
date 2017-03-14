
import Control.Monad (forM_)
import Data.List (sortBy)
import Data.Ord (comparing)

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





display :: [Counter] -> Rota -> IO ()
display allCounters rota = do
    putStrLn ""
    putStrLn $ "Score: " ++ (show (overallStrikes allCounters rota))
    putStrLn (replicate 72 '=')
    putStrLn "            Counters              Not Available"
    putStrLn (replicate 72 '-')
    let (Rota slotmap) = rota
    forM_ slotmap $ \(slot, theseCounters) -> do
        let Slot size description prefs = slot
        putStrLn $ (padRight 10 description)
                    ++ "  "
                    ++ (padLeft 22 (showNames theseCounters))
                    ++ (showNames (available prefs counters))
    putStrLn (replicate 72 '=')
  where available prefs counters = filter (not . (canDo prefs)) counters

-- BACKPORTED
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

main :: IO ()
main = do
    let rotas = sortOn (overallStrikes counters) $ usableRotas counters slots
    forM_ (take 5 rotas) $ \rota -> display counters rota
    --putStrLn $ show $ length rotas
