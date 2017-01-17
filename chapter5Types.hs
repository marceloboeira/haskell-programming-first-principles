
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

