
-- 9.1 Lists
-- a) a way ot refer to and process a collection or plurality of values
-- b) infinite series of values which allows to act as a stream

-- 9.2 List datatype

-- data [] a = [] | a : [a]

-- 9.3 Pattern matching on lists

myHead (x : _) = x
myTail (_ : xs) = xs

-- These can't deal with empty lists- this can be solved with a base case or better yet Maybe

safeTail :: [a] -> Maybe [a]
safeTail []       = Nothing
safeTail (x : []) = Nothing
safeTail (_ : xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

-- 9.4 List's syntactic sugar

-- 9.5 Using ranges to construct lists

-- Exercises: EnumFromTo

eftBool :: Bool -> Bool -> [Bool]
eftBool start stop = enumFromTo start stop

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd start stop = enumFromTo start stop

eftInt :: Int -> Int -> [Int]
eftInt start stop = enumFromTo start stop

eftChar :: Char -> Char -> [Char]
eftChar start stop = enumFromTo start stop

-- Extracting portions of lists

-- take :: Int -> [a] -> [a]
-- drop :: Int -> [a] -> [a]
-- splitAt :: Int -> [a] -> ([a], [a])

-- Exercisees : The Fearful Symmetry

myWords :: String -> [String]
myWords [] = []
myWords s  = takeWhile (/= ' ') s : myWords (dropWhile (== ' ') (dropWhile (/= ' ') s))

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines [] = []
myLines s = takeWhile (/= '\n') s : myLines (dropWhile (== '\n') (dropWhile (/= '\n') s))

shouldEqual = ["Tyger Tyger, burning bright", "In the forests of the night" , "What immortal hand or eye" , "Could frame thy fearful symmetry?"]

main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)

myWords' :: String -> Char -> [String]
myWords' [] _ = []
myWords' s break  = takeWhile (/= break) s : myWords' (dropWhile (== break) (dropWhile (/= break) s)) break

-- 9.7 List Comprehensions


