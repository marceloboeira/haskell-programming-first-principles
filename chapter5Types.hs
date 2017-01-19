
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

-- 5.6 Polymorphism - functions that can accept arguments and return results of different types without having to write variations on the same expression for each type

-- parametrically polymorphic example : id :: a -> a (no typeclass)

-- Exercises: Parametricity
-- 1) id :: a -> a - can't make this function do anything else than return the input

-- testF :: (Num a) => a -> a
-- testF x = x + x

-- 2) find the only two implementations

testP :: a -> a -> a
testP x y = x

testP' :: a -> a -> a
testP' x y = y

-- 3) implement a -> b -> b

testR :: a -> b -> b
testR x y = y

-- Polymorphic Constants
-- work around function returning Int type: length function returns an int, can't be used for division

divideByLength :: (Fractional a) => a -> [Int] -> a
divideByLength numerator list = numerator / (fromIntegral (length list))

-- why does numerator have to be specified as fractional and fromIntegral does not?

--Type inference


-- f :: Num a => a -> a -> a
-- f x y = x + y + 3

-- Exercises:

-- 1) (++) :: [a] -> [a] -> [a]

-- myConcat x = x ++ " yo"
-- type is restricted to [Char] in this example

-- 2) (*) :: Num a => a -> a -> a
-- myMult x = (x / 3) * 5
-- x must be a fractional in this example

-- 3) take :: Int -> [a] -> [a]
-- myTake x = take x "hey you"
-- type of x is restricted to Int, while the second input is a String

-- 4) (>) :: Ord a => a -> a -> Bool
-- myCom x = x > (length [1..10])
-- x must be an orderable type, such as Int, which the second input is already

-- 5) (<) :: Ord a => a -> a -> Bool
-- myAlph x = x < 'z'
-- x must be some orderable type, like Char

-- 5.8 Asserting types for declarations

-- make sure that inputs and result may only be integers
-- let triple x = x * 3 : Integer
-- declare types locally with let and where clauses
-- triple x = tripleItYo x
--     where tripleItYo :: Integer -> Integer
--     tripleItYo y = y * 3

-- 5.9 Chapter Exercises

-- 1) A value of type [a] is a list whose elements are all of some type a

-- 2) A function of type [[a]] -> [a] could take a list of strings as an argument

-- 3) A function of type [a] -> Int -> a returns one element of type a from a list

-- 4) A function of type (a,b) -> a takes a tuple argument and returns the first value

-- --Determine the type

-- 1) a) 54 :: Num a => a
--    b) (0,"doge") :: Num t => (t, [Char])
--    c) (0 :: Integer, "doge") :: (Int, [Char])
--    d) Bool
--    e) 5 :: Int
--    f) False :: Bool

-- 2) Given
-- x = 5, y = x + 5, w = y * 10
-- w :: Num a => a

-- 3) Given
-- x = 5, y = x + 5, z y = y * 10
-- z :: Num a => a -> a

-- 4) Given
-- x = 5, y = x + 5, f = 4 / y
-- f :: Fractional a => a

-- 5) Given
-- x = "Julie", y = " <3", z = "Haskell"
-- f = x ++ y ++ z
-- f :: [Char]

-- --Does it compile
-- 1)
-- 2) x = print
-- 3) a = (+)
--    b = 5
--    c = b * 10
--    d = c * 200
-- 4)

-- --Type variable or specific type constructor?

-- 1) f :: Num a => a -> b -> Int -> Int
--    f :: constrained polymorphic -> fully polymorphic -> concrete -> concrete

-- 2) f :: Zed -> Zed -> Blah
--    f :: concrete -> concrete -> concrete

-- 3) f :: Enum b => a -> b -> C
--    f :: fully polymorphic -> constrained polymorphic -> concrete

-- 4) f :: f -> g -> C
--    f :: fully polymorphic -> fully polymorphic -> concrete

-- Write a type signature

functionH :: [a] -> a
functionH (x:_) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

-- Given type, write the function

i :: a -> a
i x = x

c :: a -> b -> a
c x y = x

c'' :: b -> a -> b
c'' x y = x

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
r (x:xs) = xs

co :: (b -> c) -> (a -> b) -> (a -> c)
co f1 f2 x = f1 (f2 x)

-- helper :: a -> b
-- helper x = x + 1

-- helper' :: b -> c
-- helper' x = x + 2

-- helper'' :: a -> c
-- helper'' x = x + 3

a :: (a -> c) -> a -> a
a f x = x

a' :: (a -> b)  -> a -> b
a' f1 x = f1 x

-- Fix it

-- 1)
fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x < y) then fstString x else sndString y
    where x = "Signin"
          y = "Somewhere"

-- 2)
main :: IO ()
main = do
  print (1 + 2)
  print 10
  print (negate (-1))
  print ((+) 0 blah)
  where blah = negate 1

-- Type Kwon Do

--1)
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = g (f x)

-- 2)
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w (q x)

-- 3)
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-- 4)
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f1 f2 x = fst (f2 (f1 x))

-- 5.10 Definitions

Polymorphism refers to type variables which may refer to more than one concrete type, usually manifested as parametric or ad-hoc polymorphism

the principle type is the most generic type which still typechecks

type inference is the ability to infer principal types from terms

Type variable allows us to refer to an unspecified type or set of types

a typeclass is a way to express faculties or interfaces that more than one datatype has in common

parametricity is the property that exists within parametric polymorphism, this states that a function will be uniform across all concrete applications

ad-hoc polymorphism is polymorphism that appies one or more typeclass constraints to what would otherwise be a paraolymorphic type variable
