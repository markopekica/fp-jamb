module Types where

-- potezi u igri
{- v1
data Move
  = Roll                 -- prvo bacanje (svih 5)
  | Reroll [Int]         -- ponovno baci kockice na zadanim indeksima (0..4)
  | WriteScore Category  -- upis bodova u kategoriju
  | Cross Category       -- precrtaj kategoriju (upiši 0)
  deriving (Show, Eq)
-}


data Category = Ones | Twos | Threes | Fours | Fives | Sixes
  deriving (Show, Eq)

type Round = Int -- šta će mi ovo?

-- bodovi jednog igrača: polje može biti prazno ili ima broj
{- v1
data ScoreCard = ScoreCard
    { ones      :: Maybe Int
    , twos      :: Maybe Int
    , threes    :: Maybe Int
    } deriving (Show, Eq)
-}

-- stanje cijele igra u jednom trenutku
data GameState = GameState
    { roundNum          :: Round    -- broj runde
    , activePlayerIndex :: Int      -- index igrača na potezu
    , totalPlayers      :: Int      -- ukupan broj igrača
    , dice              :: [Int]    -- trenutno bačene kocke (ili prazno ako nisu bačene)
    , scoreCards        :: [ScoreCard]  -- lista scorecards; jedan listić (scorecard) po igraču
    , rollsLeft         :: Int      -- koliko je bacanja preostalo u trenutnom potezu
    } deriving (Show, Eq)

-- StrategyIO: “mozak” igrača je funkcija koja gleda GameState i (u IO) odlučuje koji Move želi.
type StrategyIO = GameState -> IO Move

data Player = Player
    { playerName        :: String
    , playerStrategy    :: StrategyIO
    }




-- NOVO: stupci na listiću
data Column = Down | Up | Free
  deriving (Show, Eq)

-- Jedna “linija” (npr. za Ones) po stupcima
data Cells = Cells
  { cDown :: Maybe Int
  , cUp   :: Maybe Int
  , cFree :: Maybe Int
  } deriving (Show, Eq)

-- ScoreCard sada ima po jednu liniju (Cells) za svaku kategoriju
data ScoreCard = ScoreCard
  { ones   :: Cells
  , twos   :: Cells
  , threes :: Cells
  , fours  :: Cells
  , fives  :: Cells
  , sixes  :: Cells
  } deriving (Show, Eq)

-- potezi sada nose i stupac
data Move
  = Roll
  | Reroll [Int]
  | WriteScore Category Column
  | Cross      Category Column
  deriving (Show, Eq)

emptyCells :: Cells
emptyCells = Cells Nothing Nothing Nothing

emptyCard :: ScoreCard
emptyCard = ScoreCard emptyCells emptyCells emptyCells emptyCells emptyCells emptyCells
