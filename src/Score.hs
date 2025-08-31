module Score
  ( score
  , applyScoreAt, applyCrossAt
  , validAt
  , prettyTicket
  , finalScore
  , gameFinished
  , columnTotal
  ) where

import Data.List (sort, group)
import Types

-- koliko bodova donosi kategorija za dane kocke
score :: Category -> [Int] -> Int
score cat ds =
  case cat of
    Ones   -> sum [1 | d <- ds, d == 1]
    Twos   -> 2 * length [() | d <- ds, d == 2]
    Threes -> 3 * length [() | d <- ds, d == 3]
    Fours  -> 4 * length [() | d <- ds, d == 4]
    Fives  -> 5 * length [() | d <- ds, d == 5]
    Sixes  -> 6 * length [() | d <- ds, d == 6]
    Max    -> sum ds
    Min    -> sum ds
    Straight ->
      let s = sort ds
      in if      s == [1,2,3,4,5] then 35
         else if s == [2,3,4,5,6] then 45
         else 0
    Full   ->
      let lens = sort (map length (group (sort ds)))
      in if lens == [2,3] then 30 + sum ds else 0    -- promijeni na samo 30 ako želiš
    Poker  ->
      let mx = maximum (map length (group (sort ds)))
      in if mx >= 4 then 40 + sum ds else 0
    Yamb   ->
      let mx = maximum (map length (group (sort ds)))
      in if mx == 5 then 50 + sum ds else 0


-- helperi za Cells
getCells :: ScoreCard -> Category -> Cells
getCells sc c = case c of
  Ones   -> ones sc
  Twos   -> twos sc
  Threes -> threes sc
  Fours  -> fours sc
  Fives  -> fives sc
  Sixes  -> sixes sc
  Max    -> maxC sc
  Min    -> minC sc
  Straight -> straight sc
  Full     -> full sc
  Poker    -> poker sc
  Yamb     -> yamb sc

setCells :: ScoreCard -> Category -> Cells -> ScoreCard
setCells sc c x = case c of
  Ones   -> sc { ones   = x }
  Twos   -> sc { twos   = x }
  Threes -> sc { threes = x }
  Fours  -> sc { fours  = x }
  Fives  -> sc { fives  = x }
  Sixes  -> sc { sixes  = x }
  Max    -> sc { maxC   = x }
  Min    -> sc { minC   = x }
  Straight -> sc { straight = x }
  Full     -> sc { full     = x }
  Poker    -> sc { poker    = x }
  Yamb     -> sc { yamb     = x }

getAt :: Cells -> Column -> Maybe Int
getAt cs Down = cDown cs
getAt cs Up   = cUp cs
getAt cs Free = cFree cs

setAt :: Cells -> Column -> Int -> Cells
setAt cs Down v = cs { cDown = Just v }
setAt cs Up   v = cs { cUp   = Just v }
setAt cs Free v = cs { cFree = Just v }

-- fiksni poredak redaka u listiću (koristi se za Down/Up validaciju)
catOrder :: [Category]
catOrder =
  [ Ones, Twos, Threes, Fours, Fives, Sixes
  , Max, Min, Straight, Full, Poker, Yamb
  ]

catIx :: Category -> Int
catIx c = case lookup c (zip catOrder [0..]) of
            Just i  -> i
            Nothing -> error "Unknown category in catIx"

-- je li ćelija popunjena na točnoj lokaciji
filledAt :: ScoreCard -> Category -> Column -> Bool
filledAt sc cat col =
  case col of
    Down -> cDown (getCells sc cat) /= Nothing
    Up   -> cUp   (getCells sc cat) /= Nothing
    Free -> cFree (getCells sc cat) /= Nothing

-- pravila za dopušteni potez
validAt :: ScoreCard -> Category -> Column -> Bool
validAt sc cat col =
  let hereFree = getAt (getCells sc cat) col == Nothing
  in case col of
       Free -> hereFree
       Down ->
         hereFree &&
         all (\c' -> filledAt sc c' Down)
             [ c' | c' <- catOrder, catIx c' < catIx cat ]
       Up ->
         hereFree &&
         all (\c' -> filledAt sc c' Up)
             [ c' | c' <- catOrder, catIx c' > catIx cat ]

applyScoreAt :: ScoreCard -> Category -> Column -> [Int] -> Maybe ScoreCard
applyScoreAt sc cat col ds
  | not (validAt sc cat col) = Nothing
  | otherwise =
      let s    = score cat ds
          row  = getCells sc cat
          row' = setAt row col s
      in Just (setCells sc cat row')

applyCrossAt :: ScoreCard -> Category -> Column -> Maybe ScoreCard
applyCrossAt sc cat col
  | not (validAt sc cat col) = Nothing
  | otherwise =
      let row  = getCells sc cat
          row' = setAt row col 0
      in Just (setCells sc cat row')

-- kompaktan listić (3 stupca: down, up, free)
cell :: Maybe Int -> String
cell Nothing  = "[  ]"
cell (Just n) = let s = show n in "[" ++ (if length s==1 then " " else "") ++ s ++ "]"

pad :: Int -> String -> String
pad k s = take k (s ++ repeat ' ')

rowLine :: String -> Cells -> String
rowLine label cs =
  pad 8 label ++ "  " ++ unwords [cell (cDown cs), cell (cUp cs), cell (cFree cs)]

prettyTicket :: ScoreCard -> String
prettyTicket sc = unlines
  [ "              down  up  free"
  , rowLine "ones"     (ones sc)
  , rowLine "twos"     (twos sc)
  , rowLine "threes"   (threes sc)
  , rowLine "fours"    (fours sc)
  , rowLine "fives"    (fives sc)
  , rowLine "sixes"    (sixes sc)
  , ""
  , rowLine "max"      (maxC sc)
  , rowLine "min"      (minC sc)
  , rowLine "straight" (straight sc)
  , rowLine "full"     (full sc)
  , rowLine "poker"    (poker sc)
  , rowLine "yamb"     (yamb sc)
  ]




-- Zbroji vrijednosti za kategorije 1–6 u danom stupcu
upperSum :: ScoreCard -> Column -> Int
upperSum sc col =
  sum [ val
      | cat <- [Ones, Twos, Threes, Fours, Fives, Sixes]
      , Just val <- [lookupCell sc cat col]
      ]

-- Pomoćna: dohvat jedne ćelije (ako je upisana)
lookupCell :: ScoreCard -> Category -> Column -> Maybe Int
lookupCell sc cat col =
  let pick cs = case col of
                  Down -> cDown cs
                  Up   -> cUp cs
                  Free -> cFree cs
  in case cat of
       Ones     -> pick (ones sc)
       Twos     -> pick (twos sc)
       Threes   -> pick (threes sc)
       Fours    -> pick (fours sc)
       Fives    -> pick (fives sc)
       Sixes    -> pick (sixes sc)
       Max      -> pick (maxC sc)
       Min      -> pick (minC sc)
       Straight -> pick (straight sc)
       Full     -> pick (full sc)
       Poker    -> pick (poker sc)
       Yamb     -> pick (yamb sc)

-- Ukupan rezultat jednog stupca (gornji dio + bonus)
columnTotal :: ScoreCard -> Column -> Int
columnTotal sc col =
  let subtotal = upperSum sc col
  in if subtotal >= 60 then subtotal + 30 else subtotal

-- Ukupni rezultat: po stupcima (upper+bonus) + (Max-Min)*Ones + (Straight+Full+Poker+Yamb)
finalScore :: ScoreCard -> Int
finalScore sc =
  let cols = [Down, Up, Free]
      perCol col = columnTotal sc col           -- upper + bonus
                 + maxMinTerm sc col            -- (Max-Min)*Ones
                 + lowerOthersSum sc col        -- Straight+Full+Poker+Yamb
  in sum (map perCol cols)


-- Score.hs
gameFinished :: GameState -> Bool
gameFinished st =
  all full (scoreCards st)
  where
    full sc = allCellsFilled [ones sc, twos sc, threes sc, fours sc, fives sc, sixes sc]

allCellsFilled :: [Cells] -> Bool
allCellsFilled csList =
  all (\cs -> cDown cs /= Nothing && cUp cs /= Nothing && cFree cs /= Nothing) csList


-- Dohvati vrijednost ćelije ili 0 ako je prazna
cellVal :: ScoreCard -> Category -> Column -> Int
cellVal sc cat col = maybe 0 id (lookupCell sc cat col)

-- (Max - Min) * Ones u danom stupcu
maxMinTerm :: ScoreCard -> Column -> Int
maxMinTerm sc col =
  let maxV  = cellVal sc Max  col
      minV  = cellVal sc Min  col
      onesV = cellVal sc Ones col   -- broj jedinica u tom stupcu
  in (maxV - minV) * onesV

-- Zbroj donjeg “ostatka” (bez Max/Min): Straight+Full+Poker+Yamb
lowerOthersSum :: ScoreCard -> Column -> Int
lowerOthersSum sc col =
  sum [ cellVal sc Straight col
      , cellVal sc Full     col
      , cellVal sc Poker    col
      , cellVal sc Yamb     col
      ]