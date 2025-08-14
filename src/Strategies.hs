module Strategies (easyAI) where

import Types
import Score (score, valid)

-- pomoćna: maksimalni po f
argmax :: Ord b => (a -> b) -> [a] -> a
argmax f (x:xs) = go x (f x) xs
  where
    go best bestV []     = best
    go best bestV (y:ys) =
      let vy = f y
      in if vy > bestV then go y vy ys else go best bestV ys

-- Ako nema kockica -> baci; inače -> završetak poteza
easyAI :: StrategyIO
easyAI st
    | null (dice st) = pure Roll
    | otherwise =
        let sc  = scoreCards st !! activePlayerIndex st
            cats    = [Ones, Twos, Threes]
            freeCats    = filter (valid sc) cats
        in case freeCats of
            (c:cs) ->
                let best = argmax (\k -> score k (dice st)) freeCats
                in pure (WriteScore best)
            [] -> pure Roll