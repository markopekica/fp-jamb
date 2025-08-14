module Main where

import Types
import Engine
import Terminal (human)
import Strategies (easyAI)

main :: IO ()
main = do
    let players = [human, Player "AI" easyAI] -- redoslijed; tko prvi igra
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
        WriteScore cat -> putStrLn $ "Upisano u " ++ show cat
        Cross cat      -> putStrLn $ "Prekriženo " ++ show cat
        _              -> putStrLn $ "\ESC[32mKockice: " ++ (if null (dice st') then "[]" else show (dice st')) ++ "\ESC[0m"


    -- putStrLn $ "Preostalo bacanja: " ++ show (rollsLeft st')
    if roundNum st' > 10
        then putStr "Kraj igre!"
        else loop st' ps