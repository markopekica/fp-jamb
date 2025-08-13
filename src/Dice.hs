module Dice (rollN) where

import System.Random (randomRIO)

rollN :: Int -> IO [Int]
rollN n = sequence (replicate n (randomRIO (1,6)))
-- alternativa:
-- import Control.Monad (replicateM)
-- rollN :: Int -> IO [Int]
-- rollN n = replicateM n (randomRIO (1,6))