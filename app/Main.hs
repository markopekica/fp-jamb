module Main where

import Types
import Engine
import Terminal (human)
import Strategies (randomAI, greedyAI)  -- koristi što želiš; human ostaje za ljudskog igrača
import Score (finalScore, gameFinished, prettyTicket)
import qualified Data.Ord as Ord
import Data.List (sortOn)
import Hints (printHints)

main :: IO ()
main = do
    -- biraj sastav:  (1) čovjek vs AI     ili     (2) AI vs AI
    -- let players = [human, Player "AI" greedyAI]
    let players = [ Player "Rand"   randomAI
                  , Player "Greedy" greedyAI
                  ]
    loop (initialState (length players)) players

loop :: GameState -> [Player] -> IO ()
loop st ps = do
    putStrLn "\n----------------------"
    let p = ps !! activePlayerIndex st
    putStrLn $ "Na potezu: " ++ playerName p

    mv  <- playerStrategy p st
    st' <- stepIO st mv

    putStrLn $ "AI potez: " ++ case mv of
        Roll -> "Roll"
        Reroll is -> "Reroll " ++ show is
        WriteScore c col -> "Write " ++ show c ++ " / " ++ show col
        Cross c col -> "Cross " ++ show c ++ " / " ++ show col

    case mv of
        WriteScore cat col -> putStrLn $ "Upisano u "   ++ show cat ++ " / " ++ show col
        Cross      cat col -> putStrLn $ "Prekriženo " ++ show cat ++ " / " ++ show col
        _ -> do
          putStrLn $ "Kockice: " ++ (if null (dice st') then "[]" else show (dice st'))
          -- pokaži hintove nakon svakog bacanja / reroll-a
          printHints st'

    -- okini kraj samo na prijelazu u završeno stanje
    if (not $ gameFinished st) && gameFinished st'
        then endGame st' ps
        else loop st' ps

-- ====== kraj igre ======
endGame :: GameState -> [Player] -> IO ()
endGame st ps = do
    putStrLn "\n=== KRAJ IGRE ==="
    putStrLn "Rezultati:"
    let scs = scoreCards st
    mapM_ (\(p,sc) -> do
            putStrLn $ "\nIgrač: " ++ playerName p
            putStrLn (prettyTicket sc)
            putStrLn $ "Ukupno: " ++ show (finalScore sc)
        ) (zip ps scs)
