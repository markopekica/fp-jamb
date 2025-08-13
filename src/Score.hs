module Score (score, valid, applyScore) where

import Types

-- koliko bodova donosi kategorija za dane kocke
score :: Category -> [Int] -> Int
score cat ds =
    case cat of
        Ones    -> sum [1 | d <- ds, d == 1]
        Twos    -> 2 * length [() | d <- ds, d == 2]
        Threes  -> 3 * length [() | d <- ds, d == 3]

-- smijemo li upisati u polje na listiću
valid :: ScoreCard -> Category -> Bool
valid sc cat =
    case cat of
        Ones -> ones sc     == Nothing
        Twos -> twos sc     == Nothing
        Threes -> threes sc  == Nothing

-- probaj upisati bodove; vrati novi scorecard ili Nothing
applyScore :: ScoreCard -> Category -> [Int] -> Maybe ScoreCard
applyScore sc cat ds
    | not (valid sc cat) = Nothing
    | otherwise =
        let s = score cat ds
        in case cat of
            Ones    -> Just sc { ones =     Just s }
            Twos    -> Just sc { twos =     Just s }
            Threes  -> Just sc { threes =   Just s }