module Engine (initialState, advanceTurn, stepIO) where

import Types
import Dice (rollN)
import Score (applyScore)

-- početno stanje za N igrača
initialState :: Int -> GameState
initialState n =
    GameState
    { roundNum          = 1
    , activePlayerIndex = 0
    , totalPlayers      = n
    , dice              = []
    , scoreCards = replicate n (ScoreCard Nothing Nothing Nothing)
    }

-- nakon poteza: sljdeći igrač; ako se vraćamo na prvog -> nova runda
advanceTurn :: GameState -> GameState
advanceTurn st =
    let i' = (activePlayerIndex st + 1) `mod` totalPlayers st
        r' = if i' == 0 then roundNum st + 1 else roundNum st
    in st { activePlayerIndex = i', roundNum = r', dice = [] } -- obriši kockice za novog igrača

-- primjeni potez (sa slučajnim bacanjem)
stepIO :: GameState -> Move -> IO GameState
stepIO st mv = case mv of
    Roll -> do
        d <- rollN 5
        pure st { dice = d }
        
    EndTurn ->
        pure (advanceTurn st)
    
    WriteScore cat -> do
        let ix  = activePlayerIndex st
            scs = scoreCards st
            me  = scs !! ix
        case applyScore me cat (dice st) of
            Nothing -> pure st -- nevažeće upisivanje: ostavi sve kako je
            Just me' -> pure (advanceTurn st { scoreCards = replace ix me' scs })

-- pomoćna: zamijeni element na i-toj poziciji
replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ [x] ++ drop (i+1) xs