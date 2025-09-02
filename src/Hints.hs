module Hints
  ( bestHints
  , printHints
  ) where

import Types
import Score (validAt, score)
import Data.List (sortOn)

-- koje kategorije i stupce trenutno podržavamo (gornji dio)
cats :: [Category]
cats =
  [Ones, Twos, Threes, Fours, Fives, Sixes
  ,Max, Min, Straight, Full, Poker, Yamb
  ]

cols :: [Column]
cols = [Free, Down, Up]   -- Free prvo jer je najfleksibilniji

-- Top-K legalnih upisa s obzirom na trenutno vidljive kockice
bestHints :: GameState -> Int -> [(Category, Column, Int)]
bestHints st k =
  let sc   = scoreCards st !! activePlayerIndex st
      ds   = dice st
      opts = [ (c,col, score c ds)
             | not (null ds)
             , c <- cats, col <- cols
             , validAt sc c col
             ]
  in take k $ reverse (sortOn (\(_,_,v) -> v) opts)

-- Ispiši 3 najbolja prijedloga
printHints :: GameState -> IO ()
printHints st =
  case bestHints st 3 of
    [] -> pure ()
    hs -> do
      putStrLn "Hint (top 3 moves):"
      mapM_ (\(c,col,v) ->
               putStrLn $ "  " ++ show c ++ " / " ++ show col ++ " = " ++ show v) hs