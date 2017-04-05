{-# LANGUAGE InstanceSigs #-}

import           Control.Applicative
import           Data.Char

-- 22.1 Reader

-- 22.2 A new beginning

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

-- fmap boop doop x == (*2) ((+10) x)

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

-- reader is a way of stringing functions together when all those functions are awaiting one input from a shared environment
-- using reader allows us to avoid passing an argument around explicitly

-- warm up

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> rev <*> cap

-- 22.3 This is reader

-- Reader usually refers to Monad or Applicative instances

-- 22.4 Breaking down the Functor of Functions

-- 22.5 But uh, Reader?

newtype Reader r a = Reader { runReader :: r -> a }

-- instance Functor (Reader r) where
--   fmap :: (a -> b) -> Reader r a -> Reader r b
--   fmap f (Reader ra) = Reader $ \r -> f (ra r)

-- instance Functor (Reader r) where
--   fmap :: (a -> b) -> Reader r a -> Reader r b
--   fmap f (Reader ra) = Reader $ (f . ra)

-- Exercise: Ask

ask :: Reader a a
ask = Reader id

-- 22.6 Functions ahve an Applicative too

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName   :: DogName
  , address   :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName    :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen")
               (DogName "Papu")
               (Address "Austin")

-- without Reader

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- with Reader

getDogR :: Person -> Dog
getDogR = Dog <$> dogName  <*> address

-- Exercise: Reading Comprehension

-- 1

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 func fa fb = func <$> fa <*> fb

-- 2
-- newtype Reader r a = Reader { runReader :: r -> a }

asks :: (r -> a) -> Reader r a
asks func = Reader func

-- 3

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (f . ra)

-- <*> :: (r -> a -> b) -> (r -> a) -> (r -> b)
-- instance (Monoid a) => Applicative (Three' a) where
--   pure x = Three' mempty x x
--   Three' f f' f'' <*> Three' x y z = Three' (f <> x) (f' y) (f'' z)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ (\_ -> a)
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ rab <*> ra

-- Reader $ \r -> let
--      ab = rab r
--      a = ra r
--      in ab a

-- 22.7 The Monad of functions

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

barPlus r = (foo r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

-- the Monad Instance

-- (>>=) :: Monad m =>    ma -> (a -> m b) -> m b

-- dog example with Reader Monad and do syntax

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

-- Exercise: Reader Monad

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> let
    a = ra r -- a
    readerb = aRb a -- Reader r b
    rb = runReader readerb -- r -> b
    in rb r

-- 22.8 Reader Monad

-- you can get an Applicative from a monad but not vice versa

-- 22.9

-- you can swap in a different type or value of r for functions that you call, but not for functions that call you

