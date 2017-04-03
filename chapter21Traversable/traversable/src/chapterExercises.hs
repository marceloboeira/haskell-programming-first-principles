{-# LANGUAGE FlexibleInstances #-}

import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import           Data.Traversable
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- 21.12 Chapter Exercises

type TI = []

main = do
  let trigger = undefined :: TI (Int, Int, [Int])
  quickBatch (traversable trigger)

data Either' a b = Left' a | Right' b deriving (Eq, Ord, Show)

instance Functor (Either' a) where
  fmap _ (Left' x)  = Left' x
  fmap f (Right' y) = Right' (f y)

instance Applicative (Either' e) where
  pure = Right'
  Left' e <*> _ = Left' e
  Right' f <*> r = fmap f r

instance Foldable (Either' a) where
  foldMap _ (Left' _)  = mempty
  foldMap f (Right' y) = f y

  foldr _ z (Left' _)  = z
  foldr f z (Right' y) = f y z

instance Traversable (Either' a) where
  traverse _ (Left' x)  = pure (Left' x)
  traverse f (Right' y) = Right' <$> f y


-- 1

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

main1 = do
  let trigger = undefined :: Identity (Int, Int, [Int])
  quickBatch (traversable trigger)

-- main3 = do
--   let trigger = undefined :: Identity ((Sum Int), String, (Sum Int))
--   quickBatch $ functor trigger
--   quickBatch $ applicative trigger
--   quickBatch $ monad trigger
-- 2

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Flip Constant a) where
  fmap f (Flip (Constant x)) = (Flip (Constant (f x)))

instance Foldable (Flip Constant a) where
  foldr f z (Flip (Constant x)) = z
  foldl f z (Flip (Constant x)) = z
  foldMap f (Flip (Constant x)) = f x

instance Traversable (Flip Constant a) where
  traverse f (Flip (Constant x)) = (Flip . Constant) <$> (f x)

-- newtype K a b = K a deriving (Eq, Show)

-- instance Functor (Flip K a) where
--   fmap f (Flip (K x)) = Flip (K (f x))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Flip Constant a b) where
  arbitrary = Flip . Constant <$> arbitrary

instance (Eq a, Eq b) => EqProp (Flip Constant a b) where (=-=) = eq

main2 = do
  let trigger = undefined :: Flip Constant (Int, Int, [Int]) (Int, Int, [Int])
  quickBatch (traversable trigger)

-- 3

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep x) = Yep (f x)

instance Foldable Optional where
  foldr f z Nada    = z
  foldr f z (Yep x) = f x z
  foldl f z Nada    = z
  foldl f z (Yep x) = f z x
  foldMap f Nada    = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep x) = Yep <$> f x

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    elements [Nada, (Yep a)]

instance (Eq a) => EqProp (Optional a) where (=-=) = eq

main3 = do
  let trigger = undefined :: Optional (Int, Int, [Int])
  quickBatch (traversable trigger)

-- 4

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil           = Nil
  fmap f (Cons x tail) = Cons (f x) (fmap f tail)

instance Foldable List where
  foldr f z Nil           = z
--  foldr f z (Cons x tail) = (f x z) <> (foldr f z tail)
  foldr f z (Cons x tail) = (f x (foldr f z tail))
  foldl f z Nil           = z
--  foldl f z (Cons x tail) = (f z x) <> (foldl f z tail)
  foldl f z (Cons x tail) = (f (foldl f z tail) x)
  foldMap f Nil           = mempty
--  foldMap f (Cons x tail) = (f x) <> (foldMap f tail)
  foldMap f (Cons x tail) = (f x) <> (foldMap f tail)

instance Traversable List where
  traverse _ Nil           = pure Nil
  traverse f (Cons x tail) = Cons <$> f x <*> (traverse f tail)

-- double check this instance of traverse List

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Nil, (Cons a Nil), (Cons a (Cons b Nil))]

instance (Eq a) => EqProp (List a) where (=-=) = eq

main4 = do
  let trigger = undefined :: List (Int, Int, [Int])
  quickBatch (traversable trigger)

-- 5

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldr f acc (Three x y z) = f z acc
  foldl f acc (Three x y z) = f acc z
  foldMap f (Three x y z) = f z

instance Traversable (Three a b) where
  traverse f (Three x y z) = Three x y <$> (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

main5 = do
  let trigger = undefined :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int])
  quickBatch (traversable trigger)

-- 6

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Foldable (Three' a) where
  -- foldr f acc (Three' x y z) = (f z acc) <> (f y acc)
  -- foldl f acc (Three' x y z) = f acc z
  foldMap f (Three' x y z) = (f y) <> (f z)

instance Traversable (Three' a) where
  traverse f (Three' x y z) = Three' x <$> (f y) <*> (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three' x y z)

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

main6 = do
  let trigger = undefined :: Three' (Int, Int, [Int]) (Int, Int, [Int])
  quickBatch (traversable trigger)

-- 7

-- data S n a = S (n a) a

-- instance (Functor n) => Functor (S n) where
--   fmap f (S na a) = S (f <$> na) (f a)

-- instance Foldable (S n) where
--  foldr f acc (S na a) = (f (na) acc) <> (pure (f a acc))
--  foldl f acc (Three' x y z) = f acc z
--  foldMap f (S na a) = (f <$> na) <*> (f a)

-- instance Traversable (Three' a) where
--   traverse f (Three' x y z) = Three' x <$> (f y) <*> (f z)

-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
--   arbitrary = do
--     x <- arbitrary
--     y <- arbitrary
--     z <- arbitrary
--     return (Three' x y z)

-- instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

-- main6 = do
--   let trigger = undefined :: Three' (Int, Int, [Int]) (Int, Int, [Int])
--   quickBatch (traversable trigger)

-- 8

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty                    = Empty
  fmap f (Leaf x)                 = Leaf (f x)
  fmap f (Node (treea) x (treeb)) = Node (fmap f treea) (f x) (fmap f treeb)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node treea x treeb) = (foldMap f treea) <> (f x) <> (foldMap f treeb)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> (f x)
  traverse f (Node treea x treeb) = Node <$> (traverse f treea) <*> (f x) <*> (traverse f treeb)

-- instance Traversable (Either' a) where
--   traverse _ (Left' x)  = pure (Left' x)
--   traverse f (Right' y) = Right' <$> f y

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    elements [Empty, (Leaf x), (Node Empty x (Leaf y)), (Node (Leaf x) w (Node Empty y (Leaf z)))]

instance (Eq a) => EqProp (Tree a) where (=-=) = eq

main8 = do
  let trigger = undefined :: List (Int, Int, [Int])
  quickBatch (traversable trigger)

