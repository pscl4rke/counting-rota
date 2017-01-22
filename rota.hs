
import Control.Monad (forM_)

import Planning
import Requirements

display :: Rota -> IO ()
display (Rota slotmap) = do
    putStrLn (replicate 72 '=')
    forM_ slotmap $ \(slot, names) -> putStrLn $ (show slot) ++ ": " ++ (show names)
    putStrLn (replicate 72 '=')

main :: IO ()
main = forM_ (usableRotas counters slots) $ \rota -> display rota
--main = putStrLn $ show $ length $ usableRotas counters slots
