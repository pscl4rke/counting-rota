
import Control.Monad (forM_)

import Planning
import Requirements

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





display :: Rota -> IO ()
display (Rota slotmap) = do
    putStrLn (replicate 72 '=')
    putStrLn "            Counters              Not Available"
    putStrLn (replicate 72 '-')
    forM_ slotmap $ \(slot, theseCounters) -> do
        let Slot size description prefs = slot
        putStrLn $ (padRight 10 description)
                    ++ "  "
                    ++ (padLeft 22 (showNames theseCounters))
                    ++ (showNames (available prefs counters))
    putStrLn (replicate 72 '=')
  where available prefs counters = filter (not . (canDo prefs)) counters

main :: IO ()
main = forM_ (usableRotas counters slots) $ \rota -> display rota
--main = putStrLn $ show $ length $ usableRotas counters slots
