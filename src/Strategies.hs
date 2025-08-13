module Strategies (easyAI) where

import Types

-- Ako nema kockica -> baci; inače -> završetak poteza
easyAI :: StrategyIO
easyAI st =
    if null (dice st)
        then pure Roll
        else pure EndTurn