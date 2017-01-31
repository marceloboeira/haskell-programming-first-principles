
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

-- [ x^2 | x <- [1.10]]

-- predicates limit the elements drawn from the generator list

-- [x^2 | x <- [1..10], rem x 2 == 0]

-- multiple generator lists

-- [x^y | x <- [1..5], y <- [2,3]]

-- more conditions

-- [x^y | x <- [1..5], y <- [2,3], x^y < 200]

-- Tuples

-- [(x, y) | x <- [1,2,3], y <- [6,7]]

mySqr = [x^2 | x <- [1..5]]

-- [(x,y) | x <- mySqr, y <- [1..3], x < 4]

-- Exercises: Comprehend Thy Lists

list = [x | x <- mySqr, rem x 2 == 0]

list' = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y < 50]

list'' = take 5 [ (x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]

-- list comprehensions with strings

stringList = [x | x <- "Three Letter Acronym", elem x ['A' .. 'Z']]

acronyms xs = [x | x <- xs, elem x ['A'..'Z']]

-- Exercises: Square Cube

myCube = [y^3 | y <- [1..5]]

--1

tupleGen = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

countTuples = length tupleGen

-- 9.8 Spines and non-strict evaluation

blah = enumFromTo 'a' 'z'

-- Spines are evaluated independently of values

length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length xs

testList = [1, undefined, 3]

-- length' does not trip on undefined because it does not evaluate the contents of the list... just counts cons

-- function that will force both the spine and the values

mySum :: Num a => [a] -> a
mySum []       = 0
mySum (x : xs) = x + mySum xs

-- Exercises: Bottom Madness

-- 1 - Will the following expression return a value or bottom

bottomList = [x^y | x <- [1..5], y <- [2, undefined]]

-- this function returns bottom because it tries to evaluate an undefined

-- 2

bottomList' = take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]

-- This will work because the undefined member is not evaluated

-- 3

sumTest = sum [1, undefined, 3]
-- this does not work because undefined cannot be evaluated

--4

lengthTest = length [1,2, undefined]
-- works because members do not need to be evaluated

--5

lengthTest' = length $ [1,2,3] ++ undefined
-- won't work

--6

filterEven = take 1 $ filter even [1,2,3, undefined]
-- undefined is not evaluated, so it works

--7

filterEven' = take 1 $ filter even [1,3, undefined]
-- won't work, have to evaluated undefined

-- 8

filterOdd = take 1 $ filter odd [1,3, undefined]
-- this will work, undefined is not evaluated

-- 9

filterOdd' = take 2 $ filter odd [1, 3, undefined]
-- works, does not have to evaluate undefined member

-- 10
filterOdd'' = take 3 $ filter odd [1, 3, undefined]
-- won't work, has to evaluate undefined

-- Intermission: Is it in normal form?

-- 1 normal form

-- 2 WHNF

-- 3 normal form

-- 4 normal form

-- 5 normal form

-- 6 normal form

-- 7 neither


-- 9.9 Transforming lists of values

plusOne = map (+1) [1,2,3,4]

oneMinus = map (1-) [1,2,3,4]

plusOne' = fmap (+1) [1,2,3,4]

fmapId = fmap id [1,2,3]

mapId = map id [1,2,3]

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map f xs

-- Exercises : More Bottoms

-- 1
mB1 = take 1 $ map (+1) [undefined, 2, 3]
-- won't work

-- 2
mB2 = take 1 $ map (+1) [1, undefined, 3]
-- will work

--3
mB3 = take 2 $ map (+1) [1, undefined, 3]
-- won't work

--4
itIsMystery :: [Char] -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs
-- this function checks for the string "aeiou" in a list of strings

--5
mB5 = map (^2) [1..10]
-- results in squares of list
mB5' = map minimum [[1..10], [10..20], [20..30]]
-- results in minimum of each list... 1, 10, 20
mB5'' = map sum [[1..5], [1..5], [1..5]]
-- summates each list, 15, 15, 15
--6

-- 9.10 Filtering lists of values

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' pred (x:xs)
  | pred x = x : filter pred xs
  | otherwise = filter pred xs

-- Exercises: Filtering

--1

multiplesOf :: Integral a => a -> [a] -> [a]
multiplesOf _ [] = []
multiplesOf n (x:xs)
  | rem x n == 0 = x : multiplesOf n xs
  | otherwise = multiplesOf n xs

--2

howManyMultiples :: Integral a => a -> [a] -> Int
howManyMultiples n list = length $ multiplesOf n list

--3

articles = ["the", "a", "an"]


--removeArticles :: [Char] -> [Char]
removeArticles [] = []
removeArticles sentence = let (x:xs) = myWords sentence
