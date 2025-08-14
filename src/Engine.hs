module Engine (initialState, advanceTurn, stepIO) where
import Types
import Dice (rollN, rerollAt)
import Score (applyScore, applyCross)

initialState :: Int -> GameState
initialState n =
  GameState { roundNum   = 1
            , activePlayerIndex   = 0
            , totalPlayers    = n
            , dice       = []
            , scoreCards = replicate n (ScoreCard Nothing Nothing Nothing)
            , rollsLeft  = 3
            }

advanceTurn :: GameState -> GameState
advanceTurn st =
  let i' = (activePlayerIndex st + 1) `mod` totalPlayers st
      r' = if i' == 0 then roundNum st + 1 else roundNum st
  in st { activePlayerIndex = i', roundNum = r', dice = [], rollsLeft = 3 }

stepIO :: GameState -> Move -> IO GameState
stepIO st mv = case mv of
  Roll
    | rollsLeft st <= 0     -> pure st
    | not (null (dice st))  -> pure st                      -- već bacio; koristi Reroll
    | otherwise -> do
        d <- rollN 5
        pure st { dice = d, rollsLeft = rollsLeft st - 1 }

  Reroll idxs
    | rollsLeft st <= 0     -> pure st
    | null (dice st)        -> pure st                      -- nema što rerollati
    | otherwise -> do
        d' <- rerollAt (dice st) idxs
        pure st { dice = d', rollsLeft = rollsLeft st - 1 }

  WriteScore cat -> do
    let ix  = activePlayerIndex st
        scs = scoreCards st
        me  = scs !! ix
    case applyScore me cat (dice st) of
      Nothing  -> pure st
      Just me' ->
        let st' = st { scoreCards = replace ix me' scs }
        in pure (advanceTurn st')

  Cross cat ->
    let ix  = activePlayerIndex st
        scs = scoreCards st
        me  = scs !! ix
    in case applyCross me cat of
         Nothing  -> pure st
         Just me' ->
           let st' = st { scoreCards = replace ix me' scs }
           in pure (advanceTurn st')

replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ [x] ++ drop (i+1) xs
