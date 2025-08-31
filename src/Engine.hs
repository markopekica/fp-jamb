module Engine (initialState, advanceTurn, stepIO) where
import Types
import Dice (rollN, rerollAt)
import Score (applyScoreAt, applyCrossAt)

initialState :: Int -> GameState
initialState n =
  GameState { roundNum   = 1
            , activePlayerIndex   = 0
            , totalPlayers    = n
            , dice       = []
            --, scoreCards = replicate n (ScoreCard Nothing Nothing Nothing)
            , scoreCards = replicate n emptyCard  -- vidi Types.hs gore
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
    -- ako već imamo kockice i još imamo bacanja -> reroll svih
    | not (null (dice st)) && rollsLeft st > 0 -> do
        let n = length (dice st)
        d' <- rerollAt (dice st) [0..n-1]
        pure st { dice = d', rollsLeft = rollsLeft st - 1 }
    -- nema više bacanja -> ništa
    | rollsLeft st <= 0 -> pure st
    -- prvi roll (nema kockica)
    | otherwise -> do
        d <- rollN 5
        pure st { dice = d, rollsLeft = rollsLeft st - 1 }

  Reroll idxs
    | rollsLeft st <= 0     -> pure st
    | null (dice st)        -> pure st                      -- nema što rerollati
    | otherwise -> do
        d' <- rerollAt (dice st) idxs
        pure st { dice = d', rollsLeft = rollsLeft st - 1 }

  WriteScore cat col -> do
    let ix  = activePlayerIndex st
        scs = scoreCards st
        me  = scs !! ix
    case applyScoreAt me cat col (dice st) of
        Nothing  -> pure st
        Just me' ->
            let st' = st { scoreCards = replace ix me' scs }
            in pure (advanceTurn st')
    
{-
  WriteScore cat -> do
    let ix  = activePlayerIndex st
        scs = scoreCards st
        me  = scs !! ix
    case applyScore me cat (dice st) of
      Nothing  -> pure st
      Just me' ->
        let st' = st { scoreCards = replace ix me' scs }
        in pure (advanceTurn st')
-}
  Cross cat col ->
    let ix  = activePlayerIndex st
        scs = scoreCards st
        me  = scs !! ix
    in case applyCrossAt me cat col of
         Nothing  -> pure st
         Just me' ->
           let st' = st { scoreCards = replace ix me' scs }
           in pure (advanceTurn st')

replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ [x] ++ drop (i+1) xs
