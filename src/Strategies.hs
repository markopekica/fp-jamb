module Strategies (randomAI, greedyAI) where

import Types
import Score (validAt, score)
import System.Random (randomRIO)

-- sve kategorije i stupci koje podržavaš
cats :: [Category]
cats =
  [Ones, Twos, Threes, Fours, Fives, Sixes
  ,Max, Min, Straight, Full, Poker, Yamb
  ]

cols :: [Column]
cols = [Free, Down, Up]  -- Free prvo jer je najfleksibilniji

-- ========== RANDOM AI ==========
-- Pravilo:
-- - Ako NEMA kockica: Roll
-- - Ako NEMA više bacanja: Write u nasumično legalno polje (ili Cross ako ništa nije legalno)
-- - Inače (ima kockica + bacanja): nasumično Reroll (neke indekse) ILI Write/Cross
randomAI :: StrategyIO
randomAI st
  | null (dice st) = pure Roll
  | rollsLeft st == 0 =
      case freeSlots of
        (c,col):_ -> pure (WriteScore c col)
        []        -> case crossSlots of
                       (c,col):_ -> pure (Cross c col)
                       []        -> pure (WriteScore (head cats) Free) -- fallback (ne bi trebalo)
  | otherwise = do
      coin <- randomRIO (0 :: Int, 1)
      if coin == 0
        then do
          idxs <- randomIdxs (length (dice st))
          let idxs' = if null idxs then [0] else idxs
          pure (Reroll idxs')
        else chooseWriteOrCross
  where
    sc = scoreCards st !! activePlayerIndex st
    freeSlots  = [ (c,col) | c <- cats, col <- cols, validAt sc c col ]
    crossSlots = freeSlots

    chooseWriteOrCross =
      case freeSlots of
        []        -> case crossSlots of
                       (c,col):_ -> pure (Cross c col)
                       []        -> pure Roll  -- teoretski nedostižno
        xs        -> do k <- randomRIO (0, length xs - 1)
                        let (c,col) = xs !! k
                        pure (WriteScore c col)

    randomIdxs n = do
      picks <- mapM (\_ -> randomRIO (0 :: Int, 1)) [0..n-1]
      pure [ i | (i,b) <- zip [0..] picks, b == 1 ]

-- ========== GREEDY AI ==========
-- Ideja:
-- - Bez kockica -> Roll
-- - Ako nema bacanja -> Write najbolji (po trenutnom score-u), inače Cross prvo legalno
-- - Inače: ciljaj najbolju (cat,col); za numeričke kategorije rerollaj sve neciljane vrijednosti;
--          ako već imaš >=3 pogodaka vrijednosti, upiši odmah.
greedyAI :: StrategyIO
greedyAI st
  | null (dice st) = pure Roll
  | rollsLeft st == 0 =
      case bestSlot of
        Just (c,col,_) -> pure (WriteScore c col)
        Nothing        -> crossFirst
  | otherwise =
      case bestSlot of
        Just (c,col,_) ->
          case catTarget c of
            Just v ->
              let ds   = dice st
                  hits = length [ () | d <- ds, d == v ]
              in if hits >= 3
                    then pure (WriteScore c col)
                    else let idxs = [ i | (i,d) <- zip [0..] ds, d /= v ]
                         in pure (if null idxs then WriteScore c col else Reroll idxs)
            Nothing -> pure (WriteScore c col)
        Nothing -> crossFirst
  where
    sc = scoreCards st !! activePlayerIndex st
    ds = dice st
    slots = [ (c,col) | c <- cats, col <- cols, validAt sc c col ]
    bestSlot =
      case slots of
        [] -> Nothing
        _  -> let scored = [ (c,col, score c ds) | (c,col) <- slots ]
              in Just (maximumBy3rd scored)

    crossFirst =
      case slots of
        (c,col):_ -> pure (Cross c col)
        []        -> pure Roll

    catTarget cat = case cat of
      Ones -> Just 1; Twos -> Just 2; Threes -> Just 3
      Fours -> Just 4; Fives -> Just 5; Sixes -> Just 6
      _ -> Nothing

    maximumBy3rd (x:xs) = foldl (\best y -> if third y > third best then y else best) x xs
    third (_,_,v) = v