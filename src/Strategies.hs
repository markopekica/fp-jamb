module Strategies (randomAI, greedyAI) where

import Types
import Score (score, validAt)
import System.Random (randomRIO)

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



{-

easyAI :: StrategyIO
easyAI st
  | null (dice st) = pure Roll
  | otherwise =
      let sc       = scoreCards st !! activePlayerIndex st
          cats = [Ones, Twos, Threes, Fours, Fives, Sixes]
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

-}



-- korisno: sve kategorije i stupci na jednom mjestu
cats :: [Category]
cats = [Ones, Twos, Threes, Fours, Fives, Sixes]


-- ========================
--  RANDOM AI
-- ========================
-- Ideja:
-- - Ako nema kockica -> Roll
-- - Ako ima bacanja: 50% Reroll nasumičnih indeksa, 50% Write u nasumično slobodno polje
-- - Ako nema bacanja: Write u nasumično slobodno polje, inače Cross prvo slobodno
randomAI :: StrategyIO
randomAI st
  | null (dice st) = pure Roll
  | rollsLeft st == 0 =
      case freeSlots of
        (c,col):_ -> pure (WriteScore c col)
        []        -> case crossSlots of
                       (c,col):_ -> pure (Cross c col)
                       []        -> pure Roll  -- fallback, ne bi se trebao dogoditi
  | otherwise = do
      coin <- randomRIO (0 :: Int, 1)
      if coin == 0
        then do
          idxs <- randomIdxs (length (dice st))
          if null idxs
            then chooseWriteOrCross
            else pure (Reroll idxs)
        else chooseWriteOrCross
  where
    sc         = scoreCards st !! activePlayerIndex st
    freeSlots  = [ (c,col) | c <- cats, col <- cols, validAt sc c col ]
    -- za cross možeš koristiti isti validAt (dopušteno je križati samo prazno polje)
    crossSlots = freeSlots

    chooseWriteOrCross =
      case freeSlots of
        []        -> case crossSlots of
                       (c,col):_ -> pure (Cross c col)
                       []        -> pure Roll
        xs        -> do k <- randomRIO (0, length xs - 1)
                        let (c,col) = xs !! k
                        pure (WriteScore c col)

    randomIdxs n = do
      -- slučajno odluči za svaki indeks hoće li ići u reroll (p ~ 0.5)
      picks <- mapM (\_ -> randomRIO (0 :: Int,1)) [0..n-1]
      let sel = [ i | (i,b) <- zip [0..] picks, b == 1 ]
      -- osiguraj da ponekad nešto stvarno mijenjamo
      if null sel then do i <- randomRIO (0, n-1); pure [i] else pure sel

-- ========================
--  GREEDY AI
-- ========================
-- Ideja:
-- - Ako nema kockica -> Roll
-- - Ako ima bacanja:
--     * Odaberi (cat,col) s maksimalnim score-om na tren. kockama
--     * Ako je target u [Ones..Sixes], rerollaj sve kocke koje ne doprinose toj kategoriji
--       (npr. za Fours, rerollaj sve različite od 4)
--     * Ako je već "dovoljno dobro" (npr. postotak pogodaka >= 3 ili više) onda Write
-- - Ako nema bacanja: Write best; ako ništa slobodno, Cross prvo slobodno
greedyAI :: StrategyIO
greedyAI st
  | null (dice st) = pure Roll
  | rollsLeft st == 0 =
      case bestSlot of
        Just (c,col,_) -> pure (WriteScore c col)
        Nothing        -> crossFirst
  | otherwise =
      case bestSlot of
        Just (c,col,scVal) ->
          let ds = dice st
              targetVal = catTarget c
              hits      = length [ () | d <- ds, Just d == targetVal ]
          in case targetVal of
               -- za numeričke kategorije pokušaj popraviti ružnim rerollom
               Just v ->
                 if hits >= 3
                   then pure (WriteScore c col)  -- dovoljno dobro, zadrži
                   else
                     let idxs = [ i | (i,d) <- zip [0..] ds, d /= v ]
                     in if null idxs then pure (WriteScore c col) else pure (Reroll idxs)
               Nothing ->
                 -- za nenumeričke (ako ih dodaš kasnije) samo Write odmah
                 pure (WriteScore c col)
        Nothing -> crossFirst
  where
    sc  = scoreCards st !! activePlayerIndex st
    ds  = dice st
    slots = [ (c,col) | c <- cats, col <- cols, validAt sc c col ]
    -- najbolja slobodna kombinacija po trenutnom score-u
    bestSlot =
      case slots of
        [] -> Nothing
        _  ->
          let scored = [ (c,col, score c ds) | (c,col) <- slots ]
          in Just (maximumBy3rd scored)

    crossFirst =
      case slots of
        (c,col):_ -> pure (Cross c col)  -- prvo slobodno polje prekriži
        []        -> pure Roll

    -- vraća "target" vrijednost za numeričke kategorije (1..6); Nothing za ostalo
    catTarget cat = case cat of
      Ones -> Just 1; Twos -> Just 2; Threes -> Just 3
      Fours -> Just 4; Fives -> Just 5; Sixes -> Just 6

    maximumBy3rd (x:xs) = foldl (\best y -> if third y > third best then y else best) x xs
    third (_,_,v) = v