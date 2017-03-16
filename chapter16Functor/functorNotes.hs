

-- 16.1 Functor
-- Monoid showed us what it means to talk about an algebra and turn it into a typeclass
-- pattern is to abstract out a common pattern, make sure it follows defined laws, name it

-- Functor is about a pattern of mapping over structure
-- in grammar, negation is a functor (applying negation produces the negated version of a sentence)

-- 16.2 What's a functor - a way to apply a function over or around some structure that we don't want to alter (list will remain same length)

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- 16.3 fmap

-- 16.4 f - represents a type that implements the Functor typeclass

class Sumthin a where
  s :: a -> a
-- a has kind * because argument and result are both type a

-- class Else where
--   e :: b -> f (g a b c)

-- class Biffy where
--   slayer :: e a b -> (a -> c) -> (b -> d) -> e c d

-- class Impish v where
--   impossibleKind :: v -> v a

-- class AlsoImp v where
--   nope :: v a -> v

-- must have consistent arguments to v

-- data FixMePls = FixMe | Pls deriving (Eq, Show)

-- instance Functor FixMePls where
--   fmap = error "it doesn't matter, it won't compile"

data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe   = FixMe
  fmap f (Pls a) = Pls (f a)

-- Typeclasses and constructor classes

-- 16.5 Functor Laws

-- Identity
-- Composition -- fmap (f . g) == fmap f . fmap g
  -- fmap ((+1) . (*2)) [1..5] should equal fmap (+1) . fmap (*2) $ [1..5]

-- Structure Preservation
-- fmap :: Functor f => (a -> b) -> f a -> f b

-- 16.6 The Good, the Bad, and the Ugly

data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt         = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a)       = Matter (f a)

-- Law breaking instance:

-- instance Functor WhoCares where
--   fmap _ ItDoesnt = WhatThisIsCalled
--   fmap _ WhatThisIsCalled = ItDoesnt
--   fmap f (Matter a) = Matter (f a)

-- this does not leave the structure untouched

-- use functions to change values and structures, not functor instances

-- composition law: fmap (f . g) == fmap f . fmap g (from fmap id == id)

-- invalid Functor:
data CountingBad a = Heisenberg Int a deriving (Eq, Show)

instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg (n + 1) (f a)
-- data structure does not work with fmap

data CountingGood a = Heisenberg' Int a deriving (Eq, Show)

instance Functor CountingGood where
  fmap f (Heisenberg' n a) = Heisenberg' (n) (f a)

-- 16.7 Commonly Used Functors

replaceWithP = const 'p'

tossEmOne = fmap (+1) negate

tossEmOne' = (+1) . negate

-- (~) means "is roughly equivalent to"

lms = [Just "Ave", Nothing, Just "woohoo"]

-- replaceWithP lms (List (Maybe String) -> Char)
-- 'p'

-- fmap replaceWithP lms  (List (Maybe String) -> List Char)
-- "ppp"

-- (fmap . fmap) replaceWithP lms  (List (Maybe String) -> List (Maybe Char))
-- [Just 'p', Nothing, Just 'p']

-- (fmap . fmap . fmap) replaceWithP lms (List (Maybe String) -> List (Maybe string)
-- [Just "ppp",Nothing,Just "pppppp"]

-- (.) :: (b -> c) -> (a -> b) -> a -> c
--          fmap        fmap

-- fmap :: Functor f => (m -> n) -> f m -> f n
-- fmap :: Functor g => (x -> y) -> g x -> g y

-- (fmap . fmap) :: (Functor f, Functor g) => (m -> n) -> f (g m) -> f (g n)

-- 16.8 Transforming the unapplied type argument

-- (,) and Either recapitulated
data Two a b =
  Two a b
  deriving (Eq, Show)

data Or a b =
  First a
  | Second b
  deriving (Eq, Show)

-- These are both kind * -> * -> *, which is not compatible with functor, how do we write functor instances?

-- instance Functor Two where
--   fmap = undefined

-- instance Functor Or where
--   fmap = undefined

-- we can partially apply functions
-- we can apply type constructors to type variables that represent a type constant

--instance Functor (Two a) where
--  fmap = undefined

--instance Functor (Or a) where
--  fmap = undefined

-- now we have to write the implementations of fmap for both

-- instance Functor (Two a) where
--   fmap f (Two a b) = Two $ (f a) (f b)

-- this does not work because (a) is part of the functorial structure, (f) which we are not supposed to mess with

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- instance Functor (Two a) where
--   :: (a -> b) -> (Two z) a -> (Two z) b

-- instance Functor (Two a) where
--   fmap f (Two a b) = Two a (f b)

-- instance Functor (Or a) where
--   fmap _ (First a) = First a
--   fmap f (Second b) = Second (f b)

-- 16.9 QuickChecking Functor instances
-- fmap id = id
-- fmap (p . q) = fmap p . fmap q

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- 16.11 Ignoring Possibilities

-- Maybe

incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing  = Nothing

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust Nothing  = Nothing

-- this is redundant, sharing the Nothing case

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+1) m

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe s = fmap show s

-- this does the same thing, but we can abstract further (rewrite the without naming the arguments)

incMaybe'' :: Num a => Maybe a -> Maybe a
incMaybe'' = fmap (+1)

showMaybe'' :: Show a => Maybe a -> Maybe String
showMaybe'' = fmap show

-- this does not even have to be specific to Maybe, fmap works for all datatypes with a functor instance

--fmap (+1) :: (Functor f, Num b) => f b -> f b

liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

-- this function is now more polymorphic and more widely usable

-- Either

incIfRight :: Num a => Either e a -> Either e a
incIfRight (Right n) = Right $ n + 1
incIfRight (Left e)  = (Left e)

showIfRight :: Show a => Either e a -> Either e String
showIfRight (Right s) = Right $ show s
showIfRight (Left e)  = Left e

-- simplify

incEither :: Num a => Either e a -> Either e a
incEither m = fmap (+ 1) m

showEither :: Show a => Either e a -> Either e String
showEither s = fmap show s

-- simplify

incEither' :: Num a => Either e a -> Either e a
incEither' = fmap (+ 1)

showEither' :: Show a => Either e a -> Either e String
showEither' = fmap show

-- broaden types

liftedInc' :: (Functor f, Num b) => f b -> f b
liftedInc' = fmap (+ 1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

-- 16.12 const

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

-- b is a phantom type because it has not corresponding witness at the value/term level

-- Constant is still kind * -> * -> *
-- must apply Constant to have a functor
-- Constant a is kind * -> * and Constant a b is kind *

instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v

-- Const is a lawful Functor

-- 16.13 More Structure, More Functors

data Wrap f a = Wrap (f a) deriving (Eq, Show)

-- instance Functor (Wrap f) where
--   fmap f (Wrap fa) = Wrap (f fa)

-- instance Functor (Wrap f) where
--   fmap f (Wrap fa) = Wrap (fmap f fa)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

-- 16.14 IO Functor

getInt :: IO Int
getInt = fmap read getLine

meTooIsm :: IO String
meTooIsm = do
  input <- getLine
  return (input ++ "and me too!")

bumpIt :: IO Int
bumpIt = do
  intVal <- getInt
  return (intVal + 1)

-- 16.15 something different

-- what if we want to transform structure and leave the type argument alone
-- natural transformations

-- nat :: (f -> g) -> f a -> g a
-- this type is impossible because we cant have higher kinded types as argument types to the function type

-- type Nat f g = forall a. f a -> g a

-- 16.16 Functors are unique to a datatype
