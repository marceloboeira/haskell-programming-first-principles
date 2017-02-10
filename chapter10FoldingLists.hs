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
