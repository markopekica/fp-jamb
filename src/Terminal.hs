module Terminal (human) where

import Types
import Data.Char (toLower, isDigit)
import Score (prettyTicket, validAt)

-- ===== Player =====
human :: Player
human = Player "Vi" terminalStrategy

-- ===== Helpers =====
sanitize :: String -> String
sanitize = map toLower . filter (/= '\r')

-- parsiraj indekse iz stringa (podržava zareze i zagrade)
parseIdxs :: String -> [Int]
parseIdxs s =
  let cleaned = map (\c -> if c `elem` "()," then ' ' else c) s
  in [ read t :: Int | t <- words cleaned, all isDigit t ]

parseCategory :: String -> Maybe Category
parseCategory s = case sanitize s of
  "ones"   -> Just Ones
  "twos"   -> Just Twos
  "threes" -> Just Threes
  _        -> Nothing

parseCol :: String -> Maybe Column
parseCol s = case sanitize s of
  "down" -> Just Down
  "up"   -> Just Up
  "free" -> Just Free
  _      -> Nothing

-- ===== Strategy (UI) =====
terminalStrategy :: StrategyIO
terminalStrategy st = do
  putStrLn $ "\ESC[34mRunda: " ++ show (roundNum st) ++ "\ESC[0m"
  putStrLn "\n***** [Na potezu si] *****"

  -- listić aktivnog igrača (kratko i uvijek vidljivo)
  let sc = scoreCards st !! activePlayerIndex st
  putStrLn "\ESC[90mListić:\ESC[0m"
  putStrLn (prettyTicket sc)

  -- kockice + indeksi (ako postoje)
  let ds = dice st
  putStrLn $ "\ESC[32mKockice: " ++ (if null ds then "(nema)" else show ds) ++ "\ESC[0m"
  if null ds
    then pure ()
    else putStrLn $ "\ESC[90mIndeksi :  " ++ unwords (map show [0..length ds - 1]) ++ "\ESC[0m"

  -- izbor
  putStrLn $ "\ESC[95mPreostalo bacanja: " ++ show (rollsLeft st) ++ "\ESC[0m"
  putStrLn "\ESC[90mPotez? 1=Roll (samo prije prvog bacanja), r <indeksi 0..4>, 2=<cat> <col> (write), 3=<cat> <col> (cross)\ESC[0m"
  ask sc
  where
    ask sc = do
      line <- getLine
      let toks = words (sanitize line)
      case toks of
        -- 1) Roll svih (samo prije prvog bacanja)
        ["1"] ->
          if rollsLeft st <= 0
            then putStrLn "Nema preostalih bacanja. Odaberi 2=<cat> <col> (write) ili 3=<cat> <col> (cross)." >> ask sc
            else if not (null (dice st))
                   then putStrLn "Već si bacio. Za ponovno bacanje koristi: r <indeksi>, npr. r 0 2 4." >> ask sc
                   else pure Roll

        -- 2) Reroll po indeksima: r 0 2 4   ili   r (0,2,4)
        ("r":rest) ->
          if rollsLeft st <= 0
            then putStrLn "Nema preostalih bacanja. Odaberi 2=<cat> <col> (write) ili 3=<cat> <col> (cross)." >> ask sc
            else if null (dice st)
                   then putStrLn "Nisi još bacio. Prvo 1=Roll." >> ask sc
                   else
                     let idxsRaw = parseIdxs (unwords rest)
                         idxs    = filter (`elem` [0..4]) idxsRaw
                     in if null idxs
                          then putStrLn "Navedi indekse 0..4 (npr. r 0 1 4 ili r (0,2,4))." >> ask sc
                          else pure (Reroll idxs)

        -- 3) Write Score: 2 ones down  |  2 twos up  |  2 threes free
        ["2", catTxt, colTxt] ->
          case (parseCategory catTxt, parseCol colTxt) of
            (Just c, Just col) ->
              if null (dice st)
                then putStrLn "Nema kockica za upis (prvo roll/reroll)." >> ask sc
                else if validAt sc c col
                       then pure (WriteScore c col)
                       else putStrLn "To polje je već popunjeno." >> ask sc
            _ -> putStrLn "Primjer: 2 ones down | 2 twos up | 2 threes free" >> ask sc

        ["2"] -> do
          putStrLn "Upis: <kategorija> <stupac>  (npr: ones down / twos up / threes free)"
          l <- fmap sanitize getLine
          case words l of
            [catTxt,colTxt]
              | Just c <- parseCategory catTxt
              , Just col <- parseCol colTxt
              -> if null (dice st)
                   then putStrLn "Nema kockica za upis." >> ask sc
                   else if validAt sc c col
                          then pure (WriteScore c col)
                          else putStrLn "To polje je već popunjeno." >> ask sc
            _ -> putStrLn "Primjer: ones down" >> ask sc

        -- 4) Cross: 3 ones up | 3 twos free
        ["3", catTxt, colTxt] ->
          case (parseCategory catTxt, parseCol colTxt) of
            (Just c, Just col) ->
              if validAt sc c col
                then pure (Cross c col)
                else putStrLn "To polje je već popunjeno." >> ask sc
            _ -> putStrLn "Primjer: 3 ones up | 3 threes free" >> ask sc

        ["3"] -> do
          putStrLn "Prekriži: <kategorija> <stupac>  (npr: ones up / threes free)"
          l <- fmap sanitize getLine
          case words l of
            [catTxt,colTxt]
              | Just c <- parseCategory catTxt
              , Just col <- parseCol colTxt
              -> if validAt sc c col
                   then pure (Cross c col)
                   else putStrLn "To polje je već popunjeno." >> ask sc
            _ -> putStrLn "Primjer: twos up" >> ask sc

        -- default
        _ -> putStrLn ("Neispravan unos: " ++ show toks) >> ask sc
