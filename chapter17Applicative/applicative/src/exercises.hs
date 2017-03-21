import           Control.Applicative
--import           Data.Functor.Constant
import           Data.List
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- Exercises: Lookups

-- pure, (<$>), (<*>)

-- 1

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

-- 2

y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = liftA2 (,) y z

tupled' :: Maybe (Integer, Integer)
tupled' = (pure (,)) <*> y <*> z

liftA' :: Applicative f => (a -> b) -> f a -> f b
liftA' func fx = (pure func) <*> fx

liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2' func fx fy = (pure func) <*> fx <*> fy
--                   {   f (b -> c)    }    {f b}
--                                                 {f c}

-- 3

x :: Maybe Int
x = elemIndex 3 [1..5]

y' :: Maybe Int
y' = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = (pure max') <*> x <*> y'

-- 4

xs = [1,2,3]
ys = [4,5,6]

xx :: Maybe Integer
xx = lookup 3 $ zip xs ys

yy :: Maybe Integer
yy = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = (pure sum) <*> ((pure (,)) <*> xx <*> yy)

-- Identity - a way to introduce structure without changing the semantics of what you are doing

-- Exercise: Identity Instance

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

-- Exercise: Constant Instance - write an applicative instance

newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant m1 <*> Constant m2 = Constant (m1 <> m2)

-- Exercise: Fixer Upper

-- 1

one = const <$> Just "Hello" <*> pure "World"

-- 2

two = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]


-- List Applicative Exercise

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil           = Nil
  fmap f (Cons a (xs)) = Cons (f a) (fmap f xs)

instance Applicative List where
  pure a = Cons a (Nil)
-- list (a -> b) -> list a -> list b
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f tailf <*> Cons a taila = Cons (f a) (tailf <*> taila)

testFuncs = Cons (\i -> i + 1) (Cons (\i -> i + 2) (Nil))
testVals = Cons 1 (Cons 2 Nil)


-- ZipList Applicative Exercise

-- take' :: Int -> List a -> List a
-- take'

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
              in take' 3000 l
          ys' = let (ZipList' l) = ys
              in take' 3000 l
