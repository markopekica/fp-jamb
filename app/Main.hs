module Main where

import Types
import Engine
import Terminal (human)
import Strategies (randomAI, greedyAI)
import Score (finalScore, gameFinished, prettyTicket)
import qualified Data.Ord as Ord
import Data.List (sortOn)


main :: IO ()
main = do
    --let players = [human, Player "AI" easyAI] -- redoslijed; tko prvi igra
    --loop (initialState (length players)) players
    let players = [ Player "Rand" randomAI
                    , Player "Greedy" greedyAI
                ]
    loop (initialState (length players)) players

loop :: GameState -> [Player] -> IO ()
loop st ps = do
    putStrLn "\n----------------------"
    let p = ps !! activePlayerIndex st
    putStrLn $ "Na potezu: " ++ playerName p
    -- putStrLn $ "DBG (activeIx,totalPl): " ++ show (activePlayerIndex st, totalPlayers st)
    mv <- playerStrategy p st
    -- putStrLn $ "Odabran potez: " ++ show mv
    st' <- stepIO st mv
    -- putStrLn $ "\ESC[32mKockice: " ++ (if null (dice st') then "[]" else show (dice st')) ++ "\ESC[0m"
    case mv of
        WriteScore cat col -> putStrLn $ "Upisano u "   ++ show cat ++ " / " ++ show col
        Cross      cat col -> putStrLn $ "Prekriženo " ++ show cat ++ " / " ++ show col
        _                  -> putStrLn $ "\ESC[32mKockice: " ++ (if null (dice st') then "[]" else show (dice st')) ++ "\ESC[0m"
    {-
    case mv of
        WriteScore cat -> putStrLn $ "Upisano u " ++ show cat
        Cross cat      -> putStrLn $ "Prekriženo " ++ show cat
        _              -> putStrLn $ "\ESC[32mKockice: " ++ (if null (dice st') then "[]" else show (dice st')) ++ "\ESC[0m"
    -}

    -- putStrLn $ "Preostalo bacanja: " ++ show (rollsLeft st')
    if gameFinished st'
        then endGame st' ps
        else loop st' ps
    {--if gameFinished st'
        
        then do
            putStrLn "\n=== KRAJ IGRE ==="
            let cards   = scoreCards st'
                scores  = map finalScore cards
                pairs   = zip ps scores
                ranked = sortOn (Ord.Down . snd) pairs
            putStrLn "Poredak:"
            mapM_ (\(i,(pl,sc)) ->
                    putStrLn $ show i ++ ". " ++ playerName pl ++ " - " ++ show sc)
                (zip [1..] ranked)
        else loop st' ps
        --}
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