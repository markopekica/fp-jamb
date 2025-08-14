module Terminal (human) where

import Types
import Data.Char (toLower, isDigit)

human :: Player
human = Player "Vi" terminalStrategy

sanitize :: String -> String
sanitize = map toLower . filter (/= '\r')

-- parsiraj indekse iz stringa (podržava zareze i zagrade)
parseIdxs :: String -> [Int]
parseIdxs s =
  let cleaned = map (\c -> if c `elem` "()," then ' ' else c) s
  in [ read t :: Int | t <- words cleaned, all isDigit t ]

terminalStrategy :: StrategyIO
terminalStrategy st = do
  putStrLn $ "\ESC[34mRunda: " ++ show (roundNum st) ++ "\ESC[0m"
  putStrLn "\n***** [Na potezu si] *****"
  
  -- jedno ili drugo
  -- putStrLn $ "\ESC[32mKockice: " ++ (if null (dice st) then "(nema)" else show (dice st)) ++ "\ESC[0m"
  {-
  let ds = dice st
  putStrLn $ "\ESC[32mKockice: " ++ (if null ds then "(nema)" else show ds) ++ "\ESC[0m"
  if null ds
    then pure ()
    else putStrLn $ "\ESC[90mIndeksi :  " ++ unwords (map show [0..length ds - 1]) ++ "\ESC[0m"
  -}
  putStrLn $ "\ESC[95mPreostalo bacanja: " ++ show (rollsLeft st) ++ "\ESC[0m"
  putStrLn "\ESC[90mPotez? 1=Roll, r <indeksi 0..4>, 2=Write Score (ones/twos/threes), 3=Cross\ESC[0m"
  ask
  where
    ask = do
      line <- getLine
      let toks = words (sanitize line)
      case toks of
        -- 1) Roll svih
        ["1"] ->
          if rollsLeft st <= 0
            then putStrLn "Nema preostalih bacanja. Odaberi 2=Write Score ili 3=Cross." >> ask
            else pure Roll

        -- 2) Reroll po indeksima: r 0 2 4   ili   r (0,2,4)
        ("r":rest) ->
          if rollsLeft st <= 0
            then putStrLn "Nema preostalih bacanja. Odaberi 2=Write Score ili 3=Cross." >> ask
            else if null (dice st)
                   then putStrLn "Nisi još bacio. Prvo 1=Roll." >> ask
                   else
                     let idxsRaw = parseIdxs (unwords rest)
                         idxs     = filter (`elem` [0..4]) idxsRaw
                     in if null idxs
                          then putStrLn "Navedi indekse 0..4 (npr. r 0 1 4 ili r (0,2,4))." >> ask
                          else pure (Reroll idxs)

        -- 3) Write Score
        ["2","ones"]   -> pure (WriteScore Ones)
        ["2","twos"]   -> pure (WriteScore Twos)
        ["2","threes"] -> pure (WriteScore Threes)
        ["2"] -> do
          putStrLn "Kategorija? (ones/twos/threes)"
          c <- fmap sanitize getLine
          case c of
            "ones"   -> pure (WriteScore Ones)
            "twos"   -> pure (WriteScore Twos)
            "threes" -> pure (WriteScore Threes)
            _        -> putStrLn "Neispravan unos." >> ask

        -- 4) Cross
        ["3","ones"]   -> pure (Cross Ones)
        ["3","twos"]   -> pure (Cross Twos)
        ["3","threes"] -> pure (Cross Threes)
        ["3"] -> do
          putStrLn "Polje za prekrižiti: ones/twos/threes"
          c <- fmap sanitize getLine
          case c of
            "ones"   -> pure (Cross Ones)
            "twos"   -> pure (Cross Twos)
            "threes" -> pure (Cross Threes)
            _        -> putStrLn "Neispravan unos." >> ask

        _ -> putStrLn ("Neispravan unos: " ++ show toks) >> ask
