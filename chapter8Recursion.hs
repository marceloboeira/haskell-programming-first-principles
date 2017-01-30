import           Data.List (intersperse)

-- 8.1 Recursion - defining a function in terms of itself via self referential expressions

-- 8.2 Factorial

--This is broken because it will never stop
-- brokenFact1 :: Integer -> Integer
-- brokenFact1 n = n * brokenFact1 (n - 1)

-- correct method:

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- recurions by function composition

inc :: Num a => a -> a
inc = (+1)

three = inc . inc . inc $ 0
-- or
three' = (inc . inc . inc) 0

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n     = n
incTimes times n = 1 + (incTimes (times - 1) n)

-- abstract the recursion out of incTimes

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n-1) f b)

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n

-- Intermission: Exercise
-- applyTimes 5 (+1) 5

--(1+(1+(1+(1+(1+(5))))))

-- 8.3 Botton - refers to computations that do not successfully result in a value (usually computations that failed with an error or those that failed to terminate) bottom = False

f :: Bool -> Int
f True  = error "blah"
f False = 0

f' :: Bool -> Int
f' False = 0
-- both of these functions return errors with a True input
-- can use Maybe to bridge the gap

fMaybe :: Bool -> Maybe Int
fMaybe False = Just 0
fMaybe _     = Nothing

-- 8.4 Fibonacci Numbers

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

-- 8.5 Integral division from scratch

dividedBy :: Integer -> Integer -> Integer
dividedBy = div

-- changes to

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy' :: Numerator -> Denominator -> Quotient
dividedBy' = div

dividedBy'' :: Integral a => a -> a -> (a, a)
dividedBy'' num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

-- 8.6 Chapter Exercises

-- 1
--[[Bool]]

--2
-- b) [[3 == 3], [6 > 5], [3 < 4]]

--3
-- all of the above

--4
-- func "hello" "world"

-- Reviewing Currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"

--1
-- appedCatty "woohoo!" = "woops mrow wohoo!"
--2
-- frappe "1" = "1 mrow haha"
--3
-- frappe (appedCatty "2") = "woops mrow 2 mrow haha"
--4
-- appedCatty (frappe "blue") "woops mrow blue mrow haha"
--5
-- cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue")) = "pink mrow haha mrow green mrow woops mrow blue"
--6
-- cattyConny (flippy "Pugs" "are") "awesome" = "are mrow Pugs mrow awesome"

-- Recursion

-- 1
-- 2
sumAll :: (Eq a, Num a) => a -> a
sumAll 0 = 0
sumAll n = n + sumAll (n-1)
-- 3

multTwo :: (Integral a) => a -> a -> a
multTwo x 0 = 0
multTwo 0 y = 0
multTwo x y = x + multTwo x (y-1)

-- Fixing dividedBy
data DividedResult = Result Integer | DividedByZero

-- McCarty 91 function
-- returns x - 10 when x > 100, and 91 otherwise

mc91 :: (Ord a, Num a) => a -> a
mc91 n
  |n > 100 = n - 10
  |otherwise = mc91 (mc91 (n + 11))

-- Numbers into words

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
-- look at cases to prevent this
-- "one - two - three - four - five - six - seven - eight - nine - "
-- use the last value as the base case in the fold; instead of an empty string
