module WordNumber where

import           Data.List (intersperse)


digitToWord :: Int -> String
digitToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | otherwise = "error"

digits :: Int -> [Int]
digits 0 = []
digits n = digits (div n 10) ++ [mod n 10]

wordNumber :: Int -> [String]
wordNumber n = fmap digitToWord (digits n)

wordNumber' :: Int -> String
wordNumber' n = let wN = wordNumber n
                    in foldr (\x acc -> x ++ " - " ++ acc) "" wN
