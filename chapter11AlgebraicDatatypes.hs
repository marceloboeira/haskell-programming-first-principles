{-# LANGUAGE FlexibleInstances #-}

--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import           Data.Int
import           Data.List
-- 11.1 Algebraic datatypes

-- how to construct your own datatypes in Haskell- use to leverage pattern matching, type checking, and inference
-- a type is an enumeration of constructors that have zero or more arguments
-- sum types, product types with or without record syntax, type aliases, and newtype

-- 11.2 Data declarations review

-- data Bool = False | True

-- data [] a = [] | a : [a]

-- data constructor without arguments is called nullary constructor

-- pipe (|) denotes a "sum type" which has more than one constructor inhabiting it
-- this is opposed to a product type (data constructors have more than one argument)

-- 11.3 Data and type constructors

-- two types of constructors: type and data constructors (type used at type level, in type signatures, and typeclass declarations and instances) vs (data used at term level, values interacted with at runtime)

-- constants vs constructors : constructors with no arguments are constants
-- i e Bool is a type constants

data Trivial = Trivial'

data UnaryTypeCon a = UnaryValueCon a

--i e list datatype (data [] a = [] | a : [a]) must be applied to a concrete type before you have a list
-- "kinds" are thetypes of types - represented with *. something is fully contrete applied when kind is just *. (* -> *) has yet to be applied
-- query kind with :k

-- 11.4 Data Constructors and values

data PugType = PugData

data HuskyType a = HuskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[[[Int]]]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

-- this does not work because 10 :: Int does not agree with tye type variable - String
--badDoge :: DogueDeBordeaux String
--badDoge = DogueDeBordeaux 10

-- constructors allow us to create values of types i e DogueDebordeaux 10

data Doggies a = Husky a | Mastiff a deriving (Eq, Show)

-- if constructors don't take any arguments, they behave like constants. If they do take arguments, they behave like functions that don't do anything other than be applied

-- Exercises: Dog types

-- 1
-- Doggies is a type constructor

-- 2
-- :k Doggies :: * -> *

-- 3
-- :k Doggies String :: *

-- 4
-- :t Husky 10 :: Num a => Doggies a

-- 5
-- :t Husky (10 :: Integer) :: Doggies Interger

-- 6
-- :t Mastiff "Scooby Doo" :: Doggies [Char]

-- 7
-- DogueDeBordeaux is a type and data constructor

-- 8
-- :t DogueDeBordeaux :: DogueDebordeaux doge

-- 9
-- :t DogueDeBordeaux "doggie!" :: Doguedebordeaux [Char]


-- 11.5 What's a type and what's data?

-- Types are static and resolve at compile time (but are known before runtime, but do not persist through to runtime, at that point only worrying about data)

data Price = Price Integer deriving (Eq, Show)
--    (a)     (b)   [1]
-- type constructor (a), data constructor (b), type argument [1]

data Size = Size Integer deriving (Eq, Show)

-- Value price relies on datatype definition as well as Integer .. must be in scope

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)


-- none of these datatypes take arguments, therefore they are more like constants

data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

-- arguments to data constructors are specific types, but not specific values, cannot exclude certain values of datatypes like False...

-- Exercises: Vehicles

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 10000)

-- 1
-- :t myCar :: Vehicle

-- 2

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars list = fmap isCar list

-- 3

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu

-- 4

-- using getManu on Plane data will return an error (non exhaustive patterns)

-- 5

planeSize :: Vehicle -> Size
planeSize (Plane _ size) = size

-- 11.6 Data constructor arities

-- arity refers to the number of arguments a function takes
-- nullary - no arguments, unary - one argument...
-- data constructors that take more than one argument are called products

-- nullary
data Example0 = Example0 deriving (Eq, Show)

-- unary
data Example1 = Example1 Int deriving (Eq, Show)

-- product of Int and String
data Example2 = Example2 Int String deriving (Eq, Show)

data MyType = MyVal Int deriving (Eq, Show)
--    [1]      [2]  [3]   [4]     [5]

-- 1 Type Constructor
-- 2 Data constructor (unary)
-- 3 Type argument to [2]
-- 4 deriving clause
-- 5 derived typeclasses

-- 11.7 What makes datatypes algebraic

-- algebraic because we can describe the patterns of argument structures using (sum and product)

-- cardinality of a datatype is the number of possible values it defines, can use this to explain sum and product structures

-- ie Bool type only has two possiblities, True/False. Cardinality of Bool is 2
-- ie Int8 has a range from -128 through 127... cardinality of Int8 is 256... 256 possible runtime values

-- Exercises: Cardinality

-- 1
-- data PugType = PugData
-- cardinality = 1

-- 2
-- data Airline carinality = 3

-- 3
-- Int16 cardinality = 65536

-- 4
-- Int cardinality = very large
-- Integer cardinality cannot be measured with maxBound

-- 5
-- Int8 has a cardinality of 256 because of binary... 2^ 8 = 256


-- Simple datatypes with nullary data constructors

data Example = MakeExample deriving Show

-- Exercises: For Example

-- 1

-- :t MakeExample :: Example
-- :t Example :: Not in scope

-- 2

-- can use :info to find typeclass instances contained within example

-- 3

data Example' = MakeExample' Int deriving Show

-- :t MakeExample' :: Int -> Example'

-- Unary constructors

data Goats = Goats Int deriving (Eq, Show)

-- datatypes that contain a unary constructor (Goats) always have teh same cardinality as the type they contain (Goats has the same number of inhabitants as Int)... anything that is a valid Int must be a valid argument to Goats and vice versa

-- unary constructors are the identity function for cardinality

-- Newtype -- allows us to define a type that can only ever have a single unary data constructor (cannot be product type, sum type, or contain nullary constructors)
-- has no runtime overhead

tooManyGoats :: Int -> Bool
tooManyGoats n = n > 42

newtype Goats' = Goats' Int deriving (Eq, Show)

newtype Cows = Cows Int deriving (Eq, Show)

-- we can use this to rewrite tooManyGoats to be safer

tooManyGoats' :: Goats' -> Bool
tooManyGoats' (Goats' n) = n > 42

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- we want a special instance of TooMany for goat counting

newtype Goatss = Goatss Int deriving (Eq, Show)

instance TooMany Goatss where
  tooMany (Goatss n) = tooMany n

-- GeneralizedNewtypeDeriving
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Exercises: Logic Goats

-- 1

newtype SuperGoats = SuperGoats (Int, String) deriving (Eq, Show)

-- 2
newtype Tuple = Tuple (Int, Int)

instance TooMany Tuple where
  tooMany (Tuple (a, b)) = tooMany (a + b)

instance TooMany (Int, Int) where
  tooMany (a, b) = tooMany (a + b)

-- 3

--data Num3 a = Num3 (Num a , TooMany a)

--instance TooMany (a,TooMany a) where
--  tooMany (Num a, TooMany b) = tooMany (a + b)

-- flexible instances allows this to compile, but it fails at runtime.

-- 11.8 Sum types

-- ie data Bool = False | True

-- "|" "or" defines the sum type

-- find cardinality by adding cardinalities of data constructors ie cardinality of Bool is 2

-- Exercises: Pity the Bool

-- 1
data BigSmall = Big Bool | Small Bool deriving (Eq, Show)

-- cardinality = 4

-- 2

data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)

-- cardinality = 256 + 2 = 258

-- 11.9 Product Types

-- A product type's cardinaliyt is the product of the cardinality of its inhabitants... products let us carry multiple values around in a single data constructor (any data constructor with two or more type arguments is a product)

data QuantumBool = QuantumTrue | QuantumFalse | QuantumBoth deriving (Eq, Show)
-- cardinality of 3

data TwoQs = MkTwoQs QuantumBool QuantumBool deriving (Eq, Show)

-- can use type aliases to same effect
type TwoQs' = (QuantumBool, QuantumBool)
-- this creates a type constructor and not a data constructors
-- cardinality of 9?

-- Record Syntax - Product types with additional syntax to provide accessors

-- creating a "record" with the familiar data structure construction method
data Person = MkPerson String Int deriving (Eq, Show)

jm = MkPerson "julie" 108
ca = MkPerson "chris" 16

namae :: Person -> String
namae (MkPerson s _) = s

-- now same thing with record syntax

data Person' = Person' { name :: String, age :: Int } deriving (Eq, Show)

-- Exercises: Jammin

data Fruit = Apple | Blackberry | Peach | Plum deriving (Eq, Show, Ord)

data JamJars = Jam Fruit Int deriving (Eq, Show)

-- 2

data JammJars = Jamm { fruit :: Fruit, jars :: Int } deriving (Eq, Show, Ord)

-- 3
-- cardinality of JammJars is 4 x the range of Int16

-- 4
-- 5
row1 = Jamm Peach 10
row2 = Jamm Plum 13
row3 = Jamm Apple 2
row4 = Jamm Blackberry 30
row5 = Jamm Plum 23
row6 = Jamm Apple 30
blank = Jamm Apple (-1)
identity = [blank]

allJam = [row1, row2, row3, row4, row5, row6]

checkJars :: JammJars -> Int
checkJars (Jamm _ n) = n

checkStock :: [JammJars] -> [Int]
checkStock list = fmap checkJars list

-- 6

totalJars :: [JammJars] -> Int
totalJars list = foldr (\x y -> checkJars x + y) 0 list

-- 7

-- biggerRow :: JamJars -> JamJars -> JamJars
-- biggerRow row1 row2
--   | a > b = row1
--   | otherwise = row2
--   where (Jamm _ a) = row1
--         (Jamm _ b) = row2

biggerRow :: JammJars -> [JammJars] -> [JammJars]
biggerRow a@(Jamm _ x) b@((Jamm _ y):ys)
  | x > y = [a]
  | y > x = b
  | otherwise = [a] ++ b

mostRow :: [JammJars] -> [JammJars]
mostRow list = foldr biggerRow [blank] list

-- 8

-- 9

compareKind :: JammJars -> JammJars -> Ordering
compareKind (Jamm k _) (Jamm k' _) = compare k k'

sortJams :: [JammJars] -> [JammJars]
sortJams list = sortBy compareKind list

-- jamHelper :: JammJars -> [JammJars] -> [JammJars]
-- jamHelper a [] = [a]
-- jamHelper a@(Jamm x n) b@((Jamm y n2):ys)
--   | x < y = a : b
--   | x > y = (Jamm y n2) : a : ys
--   | n < n2 = a : b
--   | otherwise = a : b

-- foldJams :: [JammJars] -> [JammJars]
-- foldJams list = foldr jamHelper [] list

-- 10

matchJams :: JammJars -> JammJars -> Bool
matchJams (Jamm k _) (Jamm k' _) = k == k'

groupJams :: [JammJars] -> [[JammJars]]
groupJams list = groupBy matchJams sortedList
  where sortedList = sortJams list

-- list must be previously sorted for groupBy to work... cannot group discontinous groups

-- 11.10 Normal Form
