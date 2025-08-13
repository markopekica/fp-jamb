module Terminal (human) where

import Types
import Data.Char (toLower)

human :: Player
human = Player "Vi" terminalStrategy

sanitize :: String -> String
sanitize = map toLower . filter (/= '\r')

terminalStrategy :: StrategyIO
terminalStrategy st = do
  putStrLn "\n***** [Na potezu si] *****"
  putStrLn $ "Runda: " ++ show (roundNum st)
  putStrLn $ "Igrač (index): " ++ show (activePlayerIndex st)
  putStrLn $ "Kockice: " ++ (if null (dice st) then "(nema)" else show (dice st))
  putStrLn "Potez? 1=Roll, 2=End, 3=Write Ones/Twos/Threes (npr: 3 Ones)"
  line <- getLine
  let toks = map sanitize (words line)
  -- mini-debug: vidi što parser “vidi”
  -- putStrLn $ "DBG tokens: " ++ show toks
  case toks of
    ["1"]                 -> pure Roll
    ["2"]                 -> pure EndTurn
    ["3","ones"]          -> pure (WriteScore Ones)
    ["3","twos"]          -> pure (WriteScore Twos)
    ["3","threes"]        -> pure (WriteScore Threes)
    ["3"]                 -> do
                              putStrLn "Kategorija? (ones/twos/threes)"
                              c <- fmap sanitize getLine
                              case c of
                                "ones"   -> pure (WriteScore Ones)
                                "twos"   -> pure (WriteScore Twos)
                                "threes" -> pure (WriteScore Threes)
                                _        -> putStrLn "Neispravan unos." >> terminalStrategy st
    _                     -> putStrLn ("Neispravan unos: " ++ show toks) >> terminalStrategy st