--7.1
--7.2
-- Functions can be used as arguments to other functions "First class values"


myNum :: Integer
myNum = 1

myVal f = myNum + f

bindExp :: Integer -> String
bindExp x = let y = 5 in
              "the integer was: " ++ show x ++ " and y was: " ++ show y

-- This won't work

-- bindExp' :: Integer -> String
-- bindExp' x = let z = y + x in
--              let y = 5 in "The integer was: " ++ show x ++ " and y was: " ++ show y ++ " and z was: " ++ show z

-- Shadowing example
bindExp' :: Integer -> String
bindExp' x = let x = 10; y = 5 in "The integer was: " ++ show x ++ " and y was: " ++ show y

-- 7.3 Anonymous Functions

triple :: Integer -> Integer
triple x = x * 3

-- is equivalent to

--(\x -> x * 3) :: Integer -> Integer


-- Exercises:

-- 1
-- all are the same mTh x y z = x * y * z = mTh = \x -> \y -> \z -> x * y * z
mTh :: Num a => a -> a -> a -> a
mTh x y z = x * y * z
-- 2
-- mTh 3 :: Num a => a -> a -> a
-- why doesn't the input of an integer change all the types?

--3
--addOne x = x + 1
addOne = (\x -> x + 1)

-- a

addOneIfOdd n = case odd n of
  True  -> f n
  False -> n
  where f = (\x -> x + 1)

-- b

addFive = \x -> \y -> (if x > y then y else x) + 5

-- c

mflip f x y = f y x

-- 7.4 pattern matching

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False

-- module RegisteredUser where

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name)
                          (AccountNumber acctNum))
          = putStrLn $ name ++ " " ++ show acctNum

data WherePenguinsLive = Galapagos | Antarctica | Australia | SouthAfrica | SouthAmerica deriving ( Eq, Show)

data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica  = True
isSouthAfrica Galapagos    = False
isSouthAfrica Antarctica   = False
isSouthAfrica Australia    = False
isSouthAfrica SouthAmerica = False

-- is the same as

isSouthAfrica' :: WherePenguinsLive -> Bool
isSouthAfrica' SouthAfrica = True
isSouthAfrica' _           = False

-- use pattern matching to unpack Penguin

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

humboldt = Peng SouthAmerica
gentoo = Peng Antarctica
macaroni = Peng Antarctica
little = Peng Australia
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _                 = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p = (galapagosPenguin p) || (antarcticPenguin p)

-- tuples

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

addEmUp2 :: Num a => (a,a) -> a
addEmUp2 (x, y) = x + y

addEmUp2' :: Num a => (a,a) -> a
addEmUp2' tup = (fst tup) + (snd tup)

fst3 :: (a,b,c) -> a
fst3 (x, _, _) = x

third3 :: (a,b,c) -> c
third3 (_,_, x) = x

-- Exercises: variety pack

--1

k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1+2))
k3 = k (3,True)

--a
-- k :: (a, b) -> a
-- k2 :: String
-- k3 returns 3

-- 2

fTuple :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
fTuple (a, _, c) (d, _ , f) = ((a, d), (c, f))

-- 7.5 Case Expressions

-- if x + 1 == 1 then "Awesome" else "wut"

funcZ x = case x + 1 == 1 of
            True  -> "Awesome"
            False -> "Wut"

pal xs = case xs == reverse xs of
           True  -> "yes"
           False -> "no"

-- is the same as

pal' xs = case y of
            True  -> "yes"
            False -> "no"
          where y = xs == reverse xs

greetIfCool :: String -> IO ()
greetIfCool coolness = case cool of
                         True  -> putStrLn "eyyyyy. What's shakin'?"
                         False -> putStrLn "pshhhhh."
                       where cool = coolness == "downright frosty yo"

-- Exercises: Case Practice

--1

--functionC x y = if (x > y) then x else y

functionC x y = case a of
                  True  -> x
                  False -> y
                where a = x > y
-- 2

--ifEvenAdd2 n = if even n then (n+2) else n

ifEvenAdd2 n = case a of
                 True  -> (n + 2)
                 False -> n
               where a = even n
-- 3
nums x = case compare x 0 of
           LT -> -1
           GT -> 1
           EQ -> 0

-- 7.6 Higher Order Functions -- functions that accept functions as arguments

data Employee = Coder | Manager | Veep | CEO deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _     = GT
codersRuleCEOsDrool _ Coder     = LT
codersRuleCEOsDrool e e'        = compare e e'

employeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
employeeRank f e e' = case f e e' of
                      GT -> reportBoss e e'
                      EQ -> putStrLn "Neither employee is the boss"
                      LT -> (flip reportBoss) e e'


-- Exercises: Artfull Dodgy

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

--2 dodgy 1 1 = 11
--3 dodgy 2 2 = 22
--4 dodgy 1 2 = 21
--5 dodgy 2 1 = 12
--6 oneIsOne 1 = 11
--7 oneIsOne 2 = 21
--8 oneIsTwo 1 = 21
--9 oneIsTwo 2 = 22
--10 oneIsOne 3 = 31
--11 oneIsTwo 3 = 23

-- 7.7 Guards

myAbs :: Integer -> Integer
myAbs x = if x < 0 then (-x) else x

-- is equivalent to

myAbs' :: Integer -> Integer
myAbs' x
  | x < 0 = (-x)
  | otherwise = x

bloodNa :: Integer -> String
bloodNa x
  | x < 135 = "too low"
  | x > 145 = "too high"
  | otherwise = "just right"

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
  | (a^2) + (b^2) == (c^2) = "Right on"
  | otherwise = "not right"

dogYrs :: (Num a, Ord a) => a -> a
dogYrs x
  | x <= 0    = 0
  | x <= 1 = x * 15
  | x <= 2 = x * 12
  | x <= 4 = x * 8
  | otherwise = x * 6

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where y = x/100

-- Exercises: Guard Duty

-- 1
-- placing otherwise first returns Fs for all inputs
-- 2
-- reordering the guards allows inputs to produce incorrect outputs, as less specific cases may precede more accurate cases
-- 3
-- True when xs is a palindrome
-- 4
-- pal can take as an argument any list that can be checked for equality
-- 5
-- pal :: Eq a => [a] -> [Char]
-- 6
-- nums returns an indication of whether its argument is a positive or negative number or zero
-- 7
-- nums can take any orderable number as an argument
-- 8
-- nums :: (Ord a, Num t, Num a) => a -> t

-- 7.8 Function composition

-- (.) :: (b-> c) -> (a -> b) -> a -> c

-- (f . g) x = f (g x), (.) is the composition operator

-- 7.9 PointFree style

-- (f. g. h) x = f (g ( h x))
-- f . g = \x -> f (g x)
-- f . g . h = \x -> f (g (h x))
-- we dont specify arguments with pointfree
-- let f = take 5 . filter odd . enumFrom
-- f 3 = [3,5,7,9,11]

-- uses both composition and pointfree styles

add' :: Int -> Int -> Int
add' x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne' :: Int -> Int
addOne' = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)

main :: IO ()
main = do
  print (0 :: Int)
  print (add' 1 0)
  print (addOne' 0)
  print (addOnePF 0)
  print ((addOne . addOne) 0)
  print ((addOnePF . addOne) 0)
  print ((addOne . addOnePF) 0)
  print ((addOnePF . addOnePF) 0)
  print (negate (addOne 0))
  print ((negate . addOne) 0)
  print ((addOne . addOne . addOne . negate . addOne) 0)

-- 7.10 Demonstrating Composition

-- 7.11 Chapter Exercises

--1 A polymorphic function may resolve to values of different types
--2 Char -> [String]
--3 (Ord a, Num a) => a -> Bool

testFunction :: Ord a => a -> a -> Bool
testFunction x y = x > y

--4 A function with the type (a -> b) -> c is a higher order function

--5

f'' :: a -> a
f'' x = x
-- f'' True :: Bool

-- Let's write code

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10
