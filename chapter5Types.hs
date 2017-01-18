
--Type Matching Exercises

-- not :: Bool -> Bool
-- length :: [a] -> Int
-- concat :: [[a]] -> [a]
-- head :: [a] -> a
-- (<) :: Ord a => a -> a -> Bool

-- 5.5 Currying

(+-+) :: Num a => a -> a -> a
(+-+) a b = a + b - b + b

-- equivalent to : (+-+) :: Numb a => a -> (a -> a)

addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

-- Binding variables to types

funcIgnoresArgs :: a -> a -> a -> String
funcIgnoresArgs x y z = "Blah"

-- Uncurrying
-- Num a => a -> a-> a ---> Num a => (a, a) -> a

nonsense :: Bool -> Integer
nonsense True  = 907
nonsense False = 22000

typicalCurriedFunction :: Integer -> Bool -> Integer
typicalCurriedFunction i b = i + (nonsense b)

uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i, b) = i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonymousAndManuallyNested :: Integer -> Bool -> Integer
anonymousAndManuallyNested = \i -> \b -> i + (nonsense b)

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

typicalUncurriedFunction :: (Integer , Bool) -> Integer
typicalUncurriedFunction = uncurry' typicalCurriedFunction

-- Exercises: Type Arguments (p. 162)

--1) f x :: Char -> Char -> Char
--2) g :: a -> b -> c -> b
--   :t g x y z :: Char when x :: Int, y :: Char, z :: String

-- 3) h :: (Num a, Num b) => a -> b -> b
--    :t h 1.0 2 :: Integer

-- 4) h :: (Num a, Num b) => a -> b -> b
--    :t h 1 (5.5 :: Double) :: Double

-- 5) jackal :: (Ord a, Eq b) => a -> b -> a
--    :t jackal "keyboard" "has the word jackal in it" :: [Char]

-- 6) jackal :: (Ord a, Eq b) => a -> b -> a
--    :t jackal "keyboard" :: (Eq b) => b -> [Char]

-- 7) kessel :: (Ord a, Num b) => a -> b -> a
--    :t kessel 1 2 :: (Ord a, Num a) => a

-- 8) kessel :: (Ord a, Num b) => a -> b -> a
--    :t kessel 1 (2 :: Integer) :: (Ord a, Num a) => a

-- 9) kessel :: (Ord a, Num b) => a -> b -> a
--    :t kessel (1 :: Integer) 2 :: Integer

-- 5.6 Polymorphism

