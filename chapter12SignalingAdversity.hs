import           Data.List

-- 12.1 Signaling Adversity

-- sometimes we can't plan for every value in a datatype to work with a program
-- Maybe type, Either type, higher-kindedness, anamorphisms

-- 12.2 How I learned to stop worrying and love Nothing

-- data Maybe a = Nothing | Just a

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n+2) else Nothing

-- Smart constructors for datatypes

type Name = String
type Age = Integer

data Person = Person Name Age deriving (Show)

-- how to keep from creating a person with an empty string... or negative age

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 = Just $ Person name age
  | otherwise = Nothing

-- mkPerson is a smart constructor, allowing us to construct values within criteria

-- 12.3 Bleating Either

-- data Either a b = Left a | Right b

data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

-- does not require Eq:
-- toString :: PersonInvalid -> String
-- toString NameEmpty = "NameEmpty"
-- toString AgeTooLow = "AgeTooLow"

-- instance Show PersonInvalid where
--   show = toString

-- requires Eq:
blah :: PersonInvalid -> String
blah pi
  | pi == NameEmpty = "NameEmpty"
  | pi == AgeTooLow = "AgeTooLow"
  | otherwise = error "wrong type"

mkPerson' :: Name -> Age -> Either PersonInvalid Person
mkPerson' name age
  | name /= "" && age >= 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | otherwise = Left AgeTooLow

-- this method does not express list of errors... can change this:

type ValidatePerson a = Either [PersonInvalid] a

-- checking functions

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
  True  -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
  True  -> Right name
  False -> Left [NameEmpty]

mkPerson'' :: Name -> Age -> ValidatePerson Person
mkPerson'' name age = mkPersonVal (nameOkay name) (ageOkay age)

mkPersonVal :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPersonVal (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPersonVal (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPersonVal (Left badName) _             = Left badName
mkPersonVal _ (Left badAge)              = Left badAge

-- 12.4 Kinds, a thousand stars in your types - kinds are types one level up
-- kinds are used to describe the types of type constructors
-- haskell has higher-kinded types
-- type constants take no arguments Int / Bool / Char

-- type with type constructor rather than type constant:
data Example a = Blah | RoofGoats | Woot a

-- Lifted and unlifted types
-- lifted type (Includes any datatype you can define yourself) is any that can be inhabited by bottom
-- Unlifted types cannot be inhabited by bottom (native machine types and raw pointers)
-- newtypes are also unlifted

-- data Maybe a = Nothing | Just a
-- Maybe is a type constructor because it takes one argument before it becomes a real type

-- Data constructors are functions

-- 12.5 Chapter Exercises

-- Determining Kinds

-- 1

id' :: a -> a
id' = undefined

-- :k :: * -> *

-- 2

r :: a -> f a
r = undefined

-- :k a :: * -> *, :k f :: *

-- String processing

-- 1

notThe :: String -> Maybe String
notThe word
  | word == "the" = Nothing
  | otherwise = Just word

stringify :: [String] -> String
stringify []     = []
stringify (x:[]) = x
stringify (x:xs) = x ++ " " ++ stringify xs

replaceThe :: String -> String
replaceThe [] = []
replaceThe sentence = case notThe x of
  Just a  -> a ++ " " ++ replaceThe (stringify xs)
  Nothing -> "a" ++ " " ++  replaceThe (stringify xs)
  where (x:xs) = words sentence

-- 2
vowels = ['a','e','i','o','u']

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = undefined

vowelThe :: String -> String -> Bool
vowelThe _ "" = False
vowelThe a b
  | a == "the" && elem (head b) vowels = True
  | otherwise = False

mapVowels :: String -> [Bool]
mapVowels sentence
  | xs == [] = [False]
  | otherwise = vowelThe x (head xs) : mapVowels (unwords xs)
  where (x:xs) = words sentence

addTrues :: Bool -> Int -> Int
addTrues b y
  | b == True = 1 + y
  | otherwise = y

countTheVowels :: String -> Int
countTheVowels list = foldr addTrues 0 (mapVowels list)

-- 3


countAllVowels :: String -> Int
countAllVowels sentence = foldr addTrues 0 (fmap (\x -> elem x vowels) sentence)
-- this does not account for capital vowels... can fix with toUpper

-- Validate the word

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe String
mkWord word
  | countAllVowels word > countAllConsonants word = Nothing
  | otherwise = Just word

consonants = ['b','c','d','f','g','h','j','k','l','m','n','p','q','r','s','t','v','w','x','y','z']

countAllConsonants :: String -> Int
countAllConsonants sentence = foldr addTrues 0 (fmap (\x -> elem x consonants) sentence)

-- It's only natural (naturals are whole numbers from zero to infinity)

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n >= 0 = Just (convert n)
  | otherwise = Nothing

convert :: Integer -> Nat
convert 0 = Zero
convert x = Succ (convert (x-1))

-- Small library for Maybe

-- 1

isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just a) = False

-- 2

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee acc _ Nothing  = acc
mayybee acc f (Just a) = f a

-- 3

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe a (Just b) = b

fromMayybe :: a -> Maybe a -> a
fromMayybe a b = mayybee a (id) b

-- 4

listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

-- 5

catMaybes :: (Eq a) => [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs)
  | x == Nothing = catMaybes xs
  | otherwise = (maybeToList x) ++ catMaybes xs

-- 6

maybeList :: Maybe [a] -> [a]
maybeList (Just [])  = []
maybeList Nothing    = []
maybeList (Just [a]) = [a]

flipMaybe :: (Eq a) => [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe a@(x:xs)
   | elem Nothing a = Nothing
   | otherwise = Just (foldr (\x y -> maybeToList x ++ y) [] a)

-- Small library for either

-- 1

lefts' :: [Either a b] -> [a]
lefts' list = foldr (\x y -> maybeToList x ++ y) [] trues
  where trues = fmap eitherLeft list

eitherLeft :: Either a b -> Maybe a
eitherLeft (Left a)  = Just a
eitherLeft (Right _) = Nothing

-- 2

rights' :: [Either a b] -> [b]
rights' list = foldr (\x y -> maybeToList x ++ y) [] trues
  where trues = fmap eitherRight list

eitherRight :: Either a b -> Maybe b
eitherRight (Left _)  = Nothing
eitherRight (Right b) = Just b

-- 3

partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' list = (lefts' list, rights' list)

-- 4

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' _ _         = Nothing

-- 5

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f1 _ (Left a)  = (f1 a)
either' _ f2 (Right b) = (f2 b)

-- 6

leftToMaybe :: Either a b -> Maybe a
leftToMaybe (Left a)  = Just a
leftToMaybe (Right _) = Nothing

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f (Right b) = Just (either' (\_ -> f b) f (Right b))

eitherFoo = either' (\_ -> "Foo") (\_ -> "Foo") (Left 1234)
eitherFoo' = either' (\_ -> "Foo") (\_ -> "Foo") (Right 12.34)

-- Unfolds -- let us build up data structures

-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
  where go :: Num a => a -> [a] -> a
        go n []     = n
        go n (x:xs) = (go (n*x) xs)

niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
  where go :: Num a => a -> [a] -> a
        go n []=    n
        go n (x:xs) = (go (n*x) xs)

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

-- Write your own iterate and unfoldr

-- 1

myIterate :: (a -> a) -> a -> [a]
myIterate f n = n : myIterate f (f n)

-- 2
pullMaybeA :: Maybe (a, b) -> [a]
pullMaybeA (Just (a, _)) = [a]
pullMaybeA Nothing       = []

pullMaybeB :: Maybe (a, b) -> b
pullMaybeB (Just (_, b)) = b
pullMaybeB Nothing       = error "no b"

myUnfoldr :: (Eq a, Eq b) => (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f start
  | f start == Nothing = []
  | otherwise = pullMaybeA (f start) ++ (myUnfoldr f (pullMaybeB (f start)))

-- test :: (b -> Maybe (a, b)) -> b  -> Bool
-- test f start
--   | f start == Nothing = True
--   | otherwise = False

-- 3

maybeA :: (a -> a) -> a -> (Maybe (a, a))
maybeA f a = Just (f a, a)
maybeA _ _ = Nothing

betterIterate :: (Eq a) => (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just (x, f x)) x

-- Finally Something other than a list

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfoldTree :: (Eq a, Eq b) => (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfoldTree f start
  | f start == Nothing = Leaf
  | otherwise = Node (unfoldTree f (pullLeft (f start))) (pullMiddle (f start)) (unfoldTree f (pullRight (f start)))

pullLeft :: Maybe (a, b, a) -> a
pullLeft (Just (a, _, _)) = a
pullLeft Nothing          = error "no left"

pullMiddle :: Maybe (a, b, a) -> b
pullMiddle (Just (_, b, _)) = b
pullMiddle Nothing          = error "no middle"

pullRight :: Maybe (a, b, a) -> a
pullRight (Just (_, _, a)) = a
pullRight Nothing          = error "no right"

-- 2

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfoldTree comp n

comp :: Integer -> Maybe (Integer, Integer, Integer)
comp 0 = Nothing
comp n = Just ((n-1), n, (n-1))

-- find out how to make this count the other direction
