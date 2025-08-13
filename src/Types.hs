module Types where

-- potezi u igri
data Move = Roll | EndTurn | WriteScore Category
    deriving (Show, Eq) -- omogućuje ispis (show), i usporedbu (==)

data Category = Ones | Twos | Threes -- proširiti
    deriving (Show, Eq)

type Round = Int -- šta će mi ovo?

-- bodovi jednog igrača: polje može biti prazno ili ima broj
data ScoreCard = ScoreCard
    { ones      :: Maybe Int
    , twos      :: Maybe Int
    , threes    :: Maybe Int
    } deriving (Show, Eq)

-- stanje cijele igra u jednom trenutku
data GameState = GameState
    { roundNum          :: Round    -- broj runde
    , activePlayerIndex :: Int      -- index igrača na potezu
    , totalPlayers      :: Int      -- ukupan broj igrača
    , dice              :: [Int]    -- trenutno bačene kocke (ili prazno ako nisu bačene)
    , scoreCards        :: [ScoreCard]  -- lista scorecards; jedan listić (scorecard) po igraču
    } deriving (Show, Eq)

-- StrategyIO: “mozak” igrača je funkcija koja gleda GameState i (u IO) odlučuje koji Move želi.
type StrategyIO = GameState -> IO Move

data Player = Player
    { playerName        :: String
    , playerStrategy    :: StrategyIO
    }