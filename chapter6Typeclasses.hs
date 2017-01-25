import           Data.List

-- 6.1 typeclasses (Eq, Num, Ord, Enum, Show)

-- 6.2 What are typeclasses

-- 6.3 Back to Bool
-- instance Bounded Bool - have upper and lower bound
-- instance Enum Bool - can be enumerated
-- instance Eq Bool - can be checked for equality
-- instance Ord Bool - can be ordered
-- instance Read Bool - can be parsed from string to something else
-- instance Show Bool - renders things into strings

-- 6.4 Eq
-- 6.5 Num

divideThenAdd :: Fractional a => a -> a -> a
divideThenAdd x y = (x / y) + 1

-- 6.6 Type defaulting typeclasses

-- default Num Integer
-- default Real Integer
-- default Enum Integer
-- default Integral Integer
-- default Fractional Double
-- default RealFrac Double
-- default Floating Double
-- default RealFloat Double

-- (/) :: Fractional a => a -> a -> a -> a changes to (/) :: Double -> Double -> Double

-- div :: Integral a => a -> a -> a changes to div :: Integer -> Integer -> Integer

-- types can be made more specific, but not more general or polymorphic

-- 6.7 Ord

--Exercises: Will they work?

-- max (length [1,2,3]) (length [8,9,10,11,12])
-- This function will work because it supplies max with two Int values

-- compare (3 * 4) (3 * 5)
-- this function will work because it supplies compare with Num values

-- compare "Julie" True
-- This function will not work because String and Bool can't be compared (not the same type)

-- (5+3) > (3+6)
-- this will work because we are comparing two instances of Num typeclass

-- 6.8 Enum
-- 6.9 Show

-- data Mood = Blah

-- instance Show Mood where
--     show _ = "Blah"

-- 6.10 Read - takes a string and turns it into a thing, no way this will always work.

-- 6.11 Instances are dispatched by type
-- a typeclass defines a set of functions and/or values
-- types have instances of that typeclass
-- the instances specify the ways that type uses the functions of the typeclass

-- 6.12 Writing typeclass instances

data Trivial = Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show)

data Date =
  Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _     = False

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
   (==) (Identity v) (Identity v') = v == v'

-- Exercises: Eq Instances

-- 1

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
   (==) (TisAn x) (TisAn y) = x == y

-- 2

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two a b) (Two c d) = a == c && b == d

-- 3

data StringOrInt = TisAString String | TisAnInt Int

instance Eq StringOrInt where
    (==) (TisAString x') (TisAString y') = x' == y'
    (==) (TisAnInt x) (TisAnInt y)       = x == y
    (==) _ _                             = False

-- 4

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair v1 v2) (Pair v1' v2') = v1 == v1' && v2 == v2'

-- 5

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

-- 6

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne x') = x == x'
    (==) (ThatOne x) (ThatOne x') = x == x'
    (==) _ _                      = False

-- 7

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello x')     = x == x'
    (==) (Goodbye y) (Goodbye y') = y == y'
    (==) _ _                      = False

-- Ord Instances

instance Ord DayOfWeek where
    compare Fri Fri = EQ
    compare Fri _   = GT
    compare _ Fri   = LT
    compare _ _     = EQ

-- 6.13 Gimme More Operations

-- 6.14 Chapter Exercises

-- 1) The eq class makes equality test possible
-- 2) The Typeclass Ord is a subclass of Eq
-- 3) (>) :: Ord a => a -> a -> Bool
-- 4) in x = divMod 16 12, x is a tuple
-- 5) the typeclass Integral includes int and integer numbers

-- x :: Int -> Int
-- x blah = blah + 20

-- printIt :: IO ()
-- printIt = putStrLn (show x)

-- 1

data Person = Person Bool deriving (Show)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2

data Mood = Blah | Woot deriving (Show, Eq)

settleDown :: Mood -> Mood
settleDown x = if x == Woot
                  then Blah
                  else x

-- 3
-- a) Values of mood type are acceptable
-- b) settleDown 9 will return an error, as 9 is an Int, which does not have an instance of Mood
-- c) Blah > Woot returns and error because the type does not derive Ordering

-- 4
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool" "everywhere"
s2 = Sentence "Julie" "loves" "dogs"

-- Datatype declarations

data Rocks = Rocks String deriving (Eq, Show,Ord)
data Yeah = Yeah Bool deriving (Eq, Show,Ord)
data Papu = Papu Rocks Yeah deriving (Eq, Show, Ord)

-- 1
phew = Papu (Rocks "chases") (Yeah True)

-- 2
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- 3
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- 4
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

-- Match the types

-- 1
-- i :: Num a => a
-- i = 1
-- replace type signature with:
-- i :: a

i ::Num a => a
i = 1

-- 2

f :: Num a => a
f = 1

-- 3

f' :: Fractional a => a
f' = 1.0

-- 4

f'' :: RealFrac a => a
f'' = 1.0

-- 5

freud :: Ord a => a -> a
freud x = x

-- 6

freud' :: Int -> Int
freud' x = x

-- 7

myX = 1 :: Int
sigmund :: a -> Int
sigmund x = myX

-- 8

sigmund' :: Num a => a -> Int
sigmund' x = myX

-- 9

jung :: [Int] -> Int
jung xs = head (sort xs)

-- 10

young :: Ord a => [a] -> a
young xs = head (sort xs)

-- 11

mySort :: Ord a => [a] -> [a]
mySort = sort

signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)

-- Type-Kwon-Do 2

-- 1

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = (f x) == y

-- 2

arith :: Num b => (a -> b) -> Int -> a -> b
arith f x y = head (replicate x (f y))

-- 6.15 Definitions

-- Typeclass inheritance occurs when a typeclass has a superclass
-- Side effects are how we refer to observable actions programs may take other than compute a value(function has an effect on the world)
-- IO is the type for values whose evaluation bears the possiblity of causing side effects
-- An instance is the definition of how a typeclass will work for a given type
-- Derived instances allow obvious typeclasses like Eq to have the instances generated bases only on how the datatype is defined
