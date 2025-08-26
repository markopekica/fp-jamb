module Score
  ( score
  , applyScoreAt, applyCrossAt
  , validAt
  , prettyTicket
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

validAt :: ScoreCard -> Category -> Column -> Bool
validAt sc cat col = getAt (getCells sc cat) col == Nothing

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