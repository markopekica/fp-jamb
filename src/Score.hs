module Score
  ( score
  , applyScoreAt, applyCrossAt
  , validAt
  , prettyTicket
  , finalScore
  , gameFinished
  , columnTotal
  ) where


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


-- helperi za Cells
getCells :: ScoreCard -> Category -> Cells
getCells sc c = case c of
  Ones   -> ones sc
  Twos   -> twos sc
  Threes -> threes sc
  Fours  -> fours sc
  Fives  -> fives sc
  Sixes  -> sixes sc

setCells :: ScoreCard -> Category -> Cells -> ScoreCard
setCells sc c x = case c of
  Ones   -> sc { ones   = x }
  Twos   -> sc { twos   = x }
  Threes -> sc { threes = x }
  Fours  -> sc { fours  = x }
  Fives  -> sc { fives  = x }
  Sixes  -> sc { sixes  = x }

getAt :: Cells -> Column -> Maybe Int
getAt cs Down = cDown cs
getAt cs Up   = cUp cs
getAt cs Free = cFree cs

setAt :: Cells -> Column -> Int -> Cells
setAt cs Down v = cs { cDown = Just v }
setAt cs Up   v = cs { cUp   = Just v }
setAt cs Free v = cs { cFree = Just v }

-- indeks kategorije 0..5
catIx :: Category -> Int
catIx c = case c of
  Ones   -> 0; Twos  -> 1; Threes -> 2
  Fours  -> 3; Fives -> 4; Sixes  -> 5

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
             [ c' | c' <- [Ones, Twos, Threes, Fours, Fives, Sixes]
                  , catIx c' < catIx cat ]
       Up ->
         hereFree &&
         all (\c' -> filledAt sc c' Up)
             [ c' | c' <- [Ones, Twos, Threes, Fours, Fives, Sixes]
                  , catIx c' > catIx cat ]

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
  , rowLine "ones"   (ones sc)
  , rowLine "twos"   (twos sc)
  , rowLine "threes" (threes sc)
  , rowLine "fours"  (fours sc)
  , rowLine "fives"  (fives sc)
  , rowLine "sixes"  (sixes sc)
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
  case cat of
    Ones   -> pick (ones sc)
    Twos   -> pick (twos sc)
    Threes -> pick (threes sc)
    Fours  -> pick (fours sc)
    Fives  -> pick (fives sc)
    Sixes  -> pick (sixes sc)
  where
    pick cs = case col of
                Down -> cDown cs
                Up   -> cUp cs
                Free -> cFree cs

-- Ukupan rezultat jednog stupca (gornji dio + bonus)
columnTotal :: ScoreCard -> Column -> Int
columnTotal sc col =
  let subtotal = upperSum sc col
  in if subtotal >= 60 then subtotal + 30 else subtotal

-- Ukupan rezultat cijelog listića (sva 3 stupca)
finalScore :: ScoreCard -> Int
finalScore sc =
  sum [ columnTotal sc Down
      , columnTotal sc Up
      , columnTotal sc Free ]


-- Score.hs
gameFinished :: GameState -> Bool
gameFinished st =
  all full (scoreCards st)
  where
    full sc = allCellsFilled [ones sc, twos sc, threes sc, fours sc, fives sc, sixes sc]

allCellsFilled :: [Cells] -> Bool
allCellsFilled csList =
  all (\cs -> cDown cs /= Nothing && cUp cs /= Nothing && cFree cs /= Nothing) csList