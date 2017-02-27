module Main where

main :: IO ()
main = do
  putStrLn "hello world"

data Game = Game Int HighLow Guesses deriving (Eq, Show)

data HighLow = High | Low deriving (Eq, Show)

type Guesses = Int

pullHighLow :: Game -> [Char]
pullHighLow (Game _ highLow _)
  | highLow == High = "high"
  | highLow == Low = "low"

runGame :: Game -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  putStrLn $ "Last guess was " ++ pullHighLow puzzle
