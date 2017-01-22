

module Scoring where

import Data.HashTable (hashString)

import Planning

overallScore :: Rota -> Int
overallScore r = fromIntegral $ hashString $ show r
