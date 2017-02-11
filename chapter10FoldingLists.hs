import           Data.List
import           Data.Time

-- 10.1 Folds (catamorphisms)
-- a means of deconstructing data

-- 10.2 Bringing you into the fold

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

listFoldr = foldr :: (a -> b -> b) -> b -> [] a -> b

-- map applis a function to each member of a list and returns a list, while a fold replaces the cons constructors with the function and reduces the list

-- 10.3 Recursive patterns - base case is identity of the function

-- 10.4 Fold Right

-- right fold because fold is right associative; it associates to the right

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f acc []     = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

-- acc is the accumulator, usually the identity of a function, i.e. 0 for (+) and 1 for (*)

foldrr :: (a -> b -> b) -> b -> [a] -> b
foldrr f acc xs =
  case xs of
    []     -> acc
    (x:xs) -> f x (foldrr f acc xs)


anon = (\_ _ -> 9001)

-- const :: a -> b -> a
-- const x _ = x

-- 10.5 Fold Left

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f acc [] = acc
-- foldl f acc (x:xs) = foldl f (f acc x) xs

--foldr (+) 0 [1..5] vs foldl (+) 0 [1..5]
--(1 +(2 +(3 +(4 +(5+0))))) vs (((((0+1)+ 2)+ 3)+ 4)+ 5)

-- arithmetic operation that is not associative (^)

--foldr (^) 2 [1..3] = 1
--(1 ^ (2 ^ (3 ^ 2)))

--foldl (^) 2 [1..3] = 64
--(((2 ^ 1) ^ 2) ^ 3)

-- folding a list into a new list:

listFoldr' = foldr (:) [] [1..3]

listFoldl' = foldl (flip (:)) [] [1..3]

-- const with foldr

-- foldr const 0 [1..5] = 1
-- does not bother with rest of fold

-- foldr (flip const) 0 [1..5] = 0
-- only evaluated acc value

-- const with foldl

foldlConst = foldl (flip const) 0 [1..5]
-- = 5
-- continuously reads each value and forgets the rest of the fold

foldlConst' = foldl const 0 [1..5]
-- = 0
-- only remembers acc value


-- Exercises: Understanding Folds

--1

foldrE1 = foldr (*) 1 [1..5]
-- will return the same result
foldlE1 = foldl (*) 1 [1..5]

-- 2

foldlE2 = foldl (flip (*)) 1 [1..3]
-- (((3 * 1) * 2) * 1) = 6
-- double check this...

-- 3

-- one difference between foldr and foldl is:
-- foldr but not foldl associates tot he right

-- 4

-- folds are catamorphisms, meaning they are used to reduce structure

-- 5

-- a
foldr5a = foldr (++) [] ["woot", "WOOT", "woot"]

-- b
foldr5b = foldr max 'a' "fear is the little death"

-- c
andHelper :: Bool -> Bool -> Bool
andHelper a b = a && b

foldr5c = foldr andHelper True [False, True]

-- foldr f acc [] = acc
-- foldr f acc (x:xs) = f x (foldr f acc xs)
-- and operator requires elements to still be in a list... foldr takes elements out of list... && operator is better suited

-- d

foldr5d = foldr (||) True [False, False, False]

-- will never return false unless acc is false and all list values are false

-- e

foldr5e = foldr ((++) . show) "" [1..5]
-- what is the error with this problem?

-- f

foldr5f = foldr const 0 [1..5]

-- g

foldr5g = foldr const 'a' "tacos"
-- h

foldr5h = foldl (flip const) 'a' "burritos"

-- i

foldr5i = foldl (flip const) 0 [1..5]

-- Unconditional Spine Recursion

-- foldl has the successive steps of the fold as its first argument, while foldr does not
-- foldr const 0 ([1..5] ++ undefined) = 1

-- foldr (flip const) 0 ([1..5] ++ undefined) -- does not work

-- foldl unconditionall evaluates the spine, but you can still selectively evaluate teh values in the list

-- foldl is generally inappropriate with lists that are or could be infinite, also less appropriate for long lists because of forced evaluation (foldl must evaluate its whole spine before its starts evaluating values in each cell)

-- most cases where you need a left fold can be handled with foldl', which is strict (forces evaluation of the values inside cons cell as it traverses spine, insead of gathering many unevaluated expressions)

-- 10.6 How to write fold functions

-- what is start value? usually the identity of the function

threeLetters = foldr (\ a b -> take 3 a ++ b) ""  ["Pizza", "Apple", "Banana"]

-- Exercises: Database Processing

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))]

-- 1

checkDate :: DatabaseItem -> Bool
checkDate (DbDate _) = True
checkDate _          = False

--check2Date :: DatabaseItem -> Bool -> Bool
--check2Date dI b = checkDate dI || b

check2Date' :: DatabaseItem -> [UTCTime] -> [UTCTime]
check2Date' (DbDate t) utcTimes = t : utcTimes
--check2Date' dbt@(DbDate _) utcTimes = dbt:utcTimes
check2Date' _ utcTimes          = utcTimes


listNonEmpty :: [a] -> Bool
listNonEmpty (_:_) = True
listNonEmpty []    = False

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate database = foldr check2Date' [] database

-- 2

check2Numbers :: DatabaseItem -> [Integer] -> [Integer]
check2Numbers (DbNumber n) numbers = n : numbers
check2Numbers _ numbers            = numbers

filterDbNumbers :: [DatabaseItem] -> [Integer]
filterDbNumbers database = foldr check2Numbers [] database

-- 3
compareDates :: UTCTime -> UTCTime -> UTCTime
compareDates d x
  | d > x = d
  | otherwise = x

dateIdentity = (UTCTime (fromGregorian 0000 0 0) (secondsToDiffTime 0))

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent database = foldr compareDates dateIdentity dates
  where dates = filterDbDate database

-- 4

sumDb :: [DatabaseItem] -> Integer
sumDb database = foldr (+) 0 nums
  where nums = filterDbNumbers database

-- 5

avgDb :: [DatabaseItem] -> Double
avgDb database = (fromIntegral . sumDb $ database)/ count
   where count = fromIntegral . length . filterDbNumbers $ database

-- 10.7 Folding and evaluation

-- foldr associates from the innermost cons cell to the outermost, while foldl recurses unconditionally to the end of the list through self-calls and then the folding function evaluates from the outermost cons cell to the innermost

-- 10.8 summary

-- foldr - the rest of the fold is an argument to the folding function you passed in
         -- works with infinite lists

-- foldl - self calls through the list, only begins to produce values after its reached the end of the list
         -- associates to left
         -- cannot be used with infinite lists
         -- nearly useless -- should be replaced with foldl'

-- 10.9 Scans

-- scans accumulate values instead of keeping values separate, but return a list of results

-- scanl (+) 1 [1..3]
-- = [1, 1 + 1, (1 + 1) + 2, ((1 + 1) + 2) + 3]
-- = [1,2,4,7]

scanll :: (a -> b -> a) -> a -> [b] -> [a]
scanll f q ls =
  q : (case ls of
         []   -> []
         x:xs -> scanll f (f q x) xs)

fibs = 1 : scanl (+) 1 fibs

fibsN x = fibs !! x

-- scans exercises

-- 1

first20 :: [Int]
first20 = take 20 fibs

-- 2
recurseLess :: Int -> [Int] -> [Int]
recurseLess _ [] = []
recurseLess n (x:xs)
  | x < n = x : recurseLess n xs
  | otherwise = []

recurseFibs100 :: [Int]
recurseFibs100 = recurseLess 100 first20

-- this recursive function is less efficient than using a function that prepends
-- appending to a list is very inefficient, prepending is efficient

lessList  :: Int -> [Int] -> [Int]
lessList _ [] = []
lessList x list
  | x < 100 = x : list
  | otherwise = list

lessThan100 :: [Int]
lessThan100 = foldr lessList [] first20

--fibs' = filter (< 100) fibs

fibs' = takeWhile (< 100) fibs

-- 3


-- 10.10 Chapter Exercises

-- 1

stops = "pbtdkg"
vowels = "aeiou"

t1 = "ab"
t2 = "ab"
-- a

triple :: a -> b -> c -> (a,b,c)
triple x y z = (x, y, z)


stopVowelStop :: [a] -> [a] -> [(a, a, a)]
stopVowelStop lista listb = [(x, y, z) | x <- lista, y <- listb, z <- lista]

-- b
firstElem :: (Char,b,c) -> Bool
firstElem (x, _, _)
  | x == 'p' = True
  | otherwise = False

stopVowelStop' :: [(Char,Char,Char)]
stopVowelStop' = filter (firstElem) (stopVowelStop stops vowels)

-- c

nouns = ["ship", "house", "dog", "cat"]
verbs = ["sink", "burns", "poops", "roars"]

nounVerbNoun :: [String] -> [String] -> [String]
nounVerbNoun lista listb = [x ++" "++ y ++" "++ z | x <- lista, y <- listb, z <- lista , x /= z]

-- 2

seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x)))
                    (length (words x))
-- finds average number of letters in words in the sentence, rounded down
-- seekritFunc "Peter likes to program"
-- 4
-- seekritFunc "Mississippi collaborates adjuncively with Tennesseeeee"
-- 10

--myFunc = foldr f z

-- direct recursion, not usiing &&

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
  if x == False
  then False
  else myAnd xs

-- direct recusion with &&

myAnd' :: [Bool] -> Bool
myAnd' []     = True
myAnd' (x:xs) = x && myAnd xs

-- fold without pointfree syntax

myAndFold :: [Bool] -> Bool
myAndFold = foldr (\a b -> if a == False then False else b) True

-- fold with pointfree syntax

myAndFold' :: [Bool] -> Bool
myAndFold' = foldr (&&) True

-- 1

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =
  if x == True
  then True
  else myOr xs

myOr' :: [Bool] -> Bool
myOr' []     = False
myOr' (x:xs) = x || myOr' xs

myOrFold :: [Bool] -> Bool
myOrFold = foldr (\a b -> if a == True then True else b) False

myOrFold' :: [Bool] -> Bool
myOrFold' = foldr (||) False

-- 2

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) =
  if f x == True then True
  else myAny f xs

myAnyFold :: (a -> Bool) -> [a] -> Bool
myAnyFold f list = foldr (\a b -> if f a == True then True else b) False list

-- 3

myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem n (x:xs) = if n == x then True else myElem n xs

myElemFold :: Eq a => a -> [a] -> Bool
myElemFold n list = foldr (\a b -> if a == n then True else b) False list

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny n list = myAny (==n) list

-- 4

myReverse :: [a] -> [a]
myReverse []   = []
myReverse list = last list : myReverse (init list)

reverseHelper :: a -> [a] -> [a]
reverseHelper x y = x : y

myReverseFold :: [a] -> [a]
myReverseFold list = foldl (flip reverseHelper) [] list
-- Review this with associativity

-- 5

myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs

myMapFold :: (a -> b) -> [a] -> [b]
myMapFold f list = foldr (\a b -> f a : b) [] list

-- 6

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
  | f x == True = x : myFilter f xs
  | otherwise = myFilter f xs

myFilterHelper :: (a -> Bool) -> a -> [a] -> [a]
myFilterHelper f x y
  | f x == True = x : y
  | otherwise = y

myFilterFold :: (a -> Bool) -> [a] -> [a]
myFilterFold f list = foldr (myFilterHelper f) [] list

-- 7

squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

squishFold :: [[a]] -> [a]
squishFold list = foldr (++) [] list

-- 8

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishHelper :: (a -> [b]) -> a -> [b] -> [b]
squishHelper f a b = f a ++ b

squishMapFold :: (a -> [b]) -> [a] -> [b]
squishMapFold f list = foldr (squishHelper f) [] list

-- 9

-- why does repl accept the previous example of (a -> [b]) when a and b are the same types, but forces me to distinguish here?

-- helper :: a -> [b]
-- helper x = [y]
--   where y = id x

-- squishAgain :: [[a]] -> [a]
-- squishAgain list = squishMapFold  [] list

-- 10

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:xs)
  | f x (head xs) == GT = x
  | otherwise = myMaximumBy f xs

-- maxHelper :: (Ordering

myMaximumByFold :: Num a => (a -> a -> Ordering) -> [a] -> a
myMaximumByFold f list = foldr (\x y -> if f x y == GT then x else y) 0 list

-- change identity so all cases work
