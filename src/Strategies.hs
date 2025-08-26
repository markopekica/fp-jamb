module Strategies (easyAI) where

import Types
import Score (score, validAt)

-- biraj prvi slobodan stupac ovim redom:
cols :: [Column]
cols = [Free, Down, Up]

-- pomoćna: maksimalni po f
argmax :: Ord b => (a -> b) -> [a] -> a
argmax f (x:xs) = go x (f x) xs
  where
    go best bestV []     = best
    go best bestV (y:ys) =
      let vy = f y
      in if vy > bestV then go y vy ys else go best bestV ys

easyAI :: StrategyIO
easyAI st
  | null (dice st) = pure Roll
  | otherwise =
      let sc       = scoreCards st !! activePlayerIndex st
          cats     = [Ones, Twos, Threes]
          -- sve (cat,col) kombinacije koje su slobodne
          slots    = [ (c,col) | c <- cats, col <- cols, validAt sc c col ]
      in case slots of
           []        -> pure (Cross Ones Free)  -- fallback ako je sve popunjeno (neće se često desiti sada)
           _         ->
             let best = maximumByScore slots
             in pure (WriteScore (fst best) (snd best))
  where
    maximumByScore :: [(Category, Column)] -> (Category, Column)
    maximumByScore (x:xs) = foldl
      (\best cand -> if val cand > val best then cand else best) x xs
    val (c,_) = score c (dice st)