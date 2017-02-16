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
-- distributive property (2 * (3 + 4)) = 2*7 = 14 is same as 2*3 + 2*4 = 14 (sum of products)

-- generalized distributive property: a * (b + c) -> (a * b) + (a * c)
-- this works in haskell too... product types distribute over sum types

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show
-- single Nullary inhabitants (Fiction and nonfiction)
data BookType = FictionBook Fiction | NonfictionBook Nonfiction deriving Show

type AuthorName = String

data Author = Author (AuthorName, BookType)

-- apply distributive propery to Author

data Author' = Fiction' AuthorName | Nonfiction' AuthorName deriving (Eq, Show)

data Expr =
    Number Int
  | Add Expr Expr
  | Minus Expr
  | Mult Expr
  | Divide Expr Expr

-- this in in normal form because it's the sum type of products
-- stricter version of same thing:

type Number = Int
type Add = (Expr, Expr)
type Minus = Expr
type Mult = (Expr, Expr)
type Divide = (Expr, Expr)

-- type Expr =
--   Either Number
--     (Either add
--       (Either minus
--         (Either Mult Divide)))

-- Exercises: How Does Your Garden Grow?

-- 1

data FlowerType = Gardenia | Daisy | Rose | Lilac deriving Show

type Gardener = String

data Garden = Garden Gardener FlowerType deriving Show

-- normal form

data NormalGarden = Gardenia' Gardener | Daisy' Gardener | Rose' Gardener | Lilac' Gardener deriving Show

-- 11.11 Constructing and deconstructing values

data GuessWhat = Chickenbutt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a | Second b deriving (Eq, Show)

data RecordProduct a b = RecordProduct { pfirst :: a, psecond :: b} deriving (Eq, Show)

-- Sum and Product

data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)

newtype NumPig = NumPig Int deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)

data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)

-- can nest products
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

-- with sum

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo = CowInfo Name Age deriving (Eq, Show)

data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)

data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

data Animal = Cow CowInfo | Pig PigInfo | Sheep SheepInfo deriving (Eq, Show)

-- or

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

-- Constructing Values

trivialValue :: GuessWhat
trivialValue = Chickenbutt

data Id' a = MkId' a deriving (Eq, Show)

idInt :: Id' Integer
idInt = MkId' 10
-- functions are values in haskell

idIdentity :: Id' (a -> a)
idIdentity = MkId' $ \x -> x

type Awesome = Bool
type Name' = String

person :: Product Name' Awesome
person = Product "Simon" True

data Twitter = Twitter deriving (Eq, Show)

data AskFm = AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

data SocialNetwork = Twitter' | AskFm' deriving (Eq, Show)

type Twitter' = String
type AskFm' = String

twitter :: Sum Twitter' AskFm'
twitter = First "Twitter"

askfm :: Sum Twitter' AskFm'
askfm = First "AskFm"

-- because we used type synonyms instead of defining datatypes, the type system did not catch this... cannot check strings

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct {pfirst = 42, psecond = 0.00001}

data OperatingSystem =
       GnuPlusLinux
     | OpenBSDPlusNevermindJustBSDStill
     | Mac
     | Windows
     deriving (Eq, Show)

data ProgrammingLanguage =
      Haskell
     |Agda
     |Idris
     |PureScript
     deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem, lang :: ProgrammingLanguage }
  deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac, lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda, os = GnuPlusLinux }

-- Exercises: Programmers

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer x y | x <- allOperatingSystems, y <- allLanguages]

--

data ThereYet = There Integer Float String Bool deriving (Eq, Show)

nope :: Float -> String -> Bool -> ThereYet
nope = There 10

notYet :: String -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet "woohoo"

yuss :: ThereYet
yuss = notQuite False

--notice progression of types
-- use this structure to avoid bottoms... allows us to partially fill in database without creating errors

-- Deconstructing Values

newtype Name'' = Name'' String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer | WheatFarmer | SoybeanFarmer deriving Show

data Farmer = Farmer Name'' Acres FarmerType deriving Show

-- breaking down this data type

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _                        = False

-- same thing using record syntax

data FarmerRec = FarmerRec { name'' :: Name'', acres :: Acres, farmerType :: FarmerType}
  deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = case farmerType  farmer of
  DairyFarmer -> True
  _           -> False

--Don't do this:

data Automobile = Null
                | Car' { make :: String, model :: String, year :: Integer}
                deriving (Eq, Show)
-- conflicts between Null valuea and used record categories
-- keep product types with record accessors separate from sum types wrapping it

data Car'' = Car'' {make' :: String, model' :: String, year' :: Integer } deriving (Eq, Show)

data Automobile' = Null' | Automobile Car''

-- 11.12 Function type is exponential
-- given a function a-> b, we can calculate the inhabitants with formula b^a
-- ie a and b are bool = 2^2 = 4
-- a -> b -> c ==> (c ^ b) ^ a ==> c ^ (b * a)

-- review arithmetic of sum types

data Quantum = Yes | No | Both deriving (Eq, Show)

-- arithmetic of sum types 3 + 3

quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Right No

quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both

quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes

quantSum5 :: Either Quantum Quantum
quantSum5 = Left No

quantSum6 :: Either Quantum Quantum
quantSum6 = Left Both

-- arithmetic of product types 3 * 3

quantProd1 :: (Quantum, Quantum)
quantProd1 = (Yes, Yes)

quantProd2 :: (Quantum, Quantum)
quantProd2 = (Yes, No)

quantProd3 :: (Quantum, Quantum)
quantProd3 = (Yes, Both)

quantProd4 :: (Quantum, Quantum)
quantProd4 = (No, Yes)

quantProd5 :: (Quantum, Quantum)
quantProd5 = (No, No)

quantProd6 :: (Quantum, Quantum)
quantProd6 = (No, Both)

quantProd7 :: (Quantum, Quantum)
quantProd7 = (Both, Yes)

quantProd8 :: (Quantum, Quantum)
quantProd8 = (Both, No)

quantProd9 :: (Quantum, Quantum)
quantProd9 = (Both, Both)

-- function type, each possible unique implementation of the function is an inhabitants
-- 3 ^ 3

quantFlip1 :: Quantum -> Quantum
quantFlip1 Yes  = Yes
quantFlip1 No   = Yes
quantFlip1 Both = Yes

quantFlip2 :: Quantum -> Quantum
quantFlip2 Yes  = Yes
quantFlip2 No   = Yes
quantFlip2 Both = No

quantFlip3 :: Quantum -> Quantum
quantFlip3 Yes  = Yes
quantFlip3 No   = Yes
quantFlip3 Both = Both

quantFlip4 :: Quantum -> Quantum
quantFlip4 Yes  = Yes
quantFlip4 No   = No
quantFlip4 Both = Yes

quantFlip5 :: Quantum -> Quantum
quantFlip5 Yes  = Yes
quantFlip5 No   = Both
quantFlip5 Both = Yes

quantFlip6 :: Quantum -> Quantum
quantFlip6 Yes  = No
quantFlip6 No   = Yes
quantFlip6 Both = Yes

quantFlip7 :: Quantum -> Quantum
quantFlip7 Yes  = Both
quantFlip7 No   = Yes
quantFlip7 Both = Yes

quantFlip8 :: Quantum -> Quantum
quantFlip8 Yes  = Both
quantFlip8 No   = Yes
quantFlip8 Both = No

quantFlip9 :: Quantum -> Quantum
quantFlip9 Yes  = Both
quantFlip9 No   = No
quantFlip9 Both = No

quantFlip10 :: Quantum -> Quantum
quantFlip10 Yes  = Both
quantFlip10 No   = No
quantFlip10 Both = Both

-- and so on through 3 ^ 3 combinations

-- Exponentiation in what order?

convert1 :: Quantum -> Bool
convert1 Yes  = True
convert1 No   = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes  = True
convert2 No   = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes  = True
convert3 No   = False
convert3 Both = False

convert4 :: Quantum -> Bool
convert4 Yes  = False
convert4 No   = False
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes  = False
convert5 No   = False
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes  = False
convert6 No   = True
convert6 Both = True

convert7 :: Quantum -> Bool
convert7 Yes  = True
convert7 No   = False
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes  = False
convert8 No   = True
convert8 Both = False

-- 8 combinations, as a-> b = b ^ a model predicts

-- Exercises: The Quad (how many unique inhabitants)

-- 1
data Quad = One | Two | Three | Four deriving (Eq, Show)

eQuad :: Either Quad Quad
eQuad = undefined

-- arithmetic sum = 4 + 4 = 8 unique inhabitants

-- 2
prodQuad :: (Quad, Quad)
prodQuad = undefined

-- product : 4 * 4 = 16 combinations

-- 3
funcQuad :: Quad -> Quad
funcQuad = undefined
-- func type: 4 ^ 4 = 256 combinations

-- 4
--prodTBool :: (Bool, Bool, Bool)
--product :: 2 * 2 * 2 = 8 combinations

-- 5
gTwo :: Bool -> Bool -> Bool
gTwo = undefined

-- func type: 2 ^ 2 ^ 2 = 16

-- 6
--gTwo :: Bool -> Quad -> Quad
--gTwo = undefined

-- func type : 2 ^ 4 ^ 4 = 65536

-- 11.3 Higher-kinded datatypes

-- kinds are the types of type constructors (encoding the number of arguments they take)
-- default in haskell is *
-- kinds are not types until they are fully applied (* -> * -> *) must be applied twice before it is a real type

data Silly a b c d = MkSilly a b c d deriving Show
-- :k Silly
--Silly :: * -> * -> * -> * -> *
-- :k Silly Int String Bool String
--Silly Int String Bool String :: *

--data EsResultFound a = EsResultFound { _version :: DocVersion, _source :: a}
--  deriving (Eq, Show)

-- 11.14 Lists are Polymorphic-- lists can contain values of any type

data Product' a b = a :&: b deriving (Eq, Show)

-- list without infix

data List a = Nil | Cons a (List a)

-- 11.15 Binary Tree - recursive data structure

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

-- Inserting into trees

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

-- Write map for BinaryTree -- write a map function for the data structure

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

-- convert binary trees to lists

--unlist :: [a] -> a
--unlist [a] = a

preorder :: BinaryTree a -> [a]
preorder Leaf                = []
preorder (Node left a right) = (a : (preorder left)) ++ preorder right

-- assuming already ordered
inorder :: BinaryTree a -> [a]
inorder Leaf                = []
inorder (Node left a right) = (inorder left) ++ [a] ++ inorder right
-- write to order nodes as read...


postorder :: BinaryTree a -> [a]
postorder Leaf                = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2,1,3]
  then putStrLn "Pass"
  else putStrLn "Fail"

testInorder :: IO ()
testInorder =
  if inorder testTree == [1,2,3]
  then putStrLn "Pass"
  else putStrLn "Fail"

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1,3,2]
  then putStrLn "Pass"
  else putStrLn "Fail"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

-- write foldr for binary tree

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f acc Leaf                = acc
foldTree f acc (Node left a right) = f a (foldTree f acc right)
-- only traverses to the right in a binary tree... believe function must accept 3 argments to traverse all branches?

-- 11.16 Chapter Exercises

-- 1
-- weekday is a type with five data constructors

-- 2
-- continued in separate exercises file
