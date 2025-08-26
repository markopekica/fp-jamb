module Dice (rollN, rerollAt) where

import System.Random (randomRIO)
import Control.Monad (replicateM)

rollN :: Int -> IO [Int]
rollN n = replicateM n (randomRIO (1,6))

-- | rerollAt dice idxs: zamijeni elemente na indeksima idxs novim bacanjima
--   Neispravni indeksi (izvan 0..length-1) ignoriraju se.
rerollAt :: [Int] -> [Int] -> IO [Int]
rerollAt ds idxs = do
  let n      = length ds
      idxs'  = uniq (filter (\i -> i >= 0 && i < n) idxs)
      k      = length idxs'
  news <- replicateM k (randomRIO (1,6))
  pure (merge ds idxs' news)
  where
    -- zadrži prvi pojavak svakog indeksa (redoslijed važan radi determinističkog trošenja 'news')
    uniq :: [Int] -> [Int]
    uniq = go []
      where
        go _ [] = []
        go seen (x:xs)
          | x `elem` seen = go seen xs
          | otherwise     = x : go (x:seen) xs

    merge :: [Int] -> [Int] -> [Int] -> [Int]
    merge xs is ns =
      [ case lookup i (zip is ns) of
          Just newVal -> newVal
          Nothing     -> oldVal
      | (i, oldVal) <- zip [0..] xs
      ]
