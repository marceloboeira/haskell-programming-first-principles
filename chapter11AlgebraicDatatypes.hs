import           Data.Int

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

instance TooMany (Int, Int) where
  tooMany (a,b) = tooMany (a + b)
