import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- 18.7 Chapter Exercises

-- 1

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure a = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where (=-=) = eq

main = do
  let trigger = undefined :: Nope (Int, String, Int)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

-- 2

data PhbtEither b a = Leftt a | Rightt b deriving (Eq, Show)

instance Functor (PhbtEither b) where
  fmap f (Leftt a)  = Leftt (f a)
  fmap _ (Rightt b) = Rightt b

instance Monoid b => Applicative (PhbtEither b) where
  pure a = Leftt a
  Leftt a <*> Leftt a' = Leftt (a a')
  Leftt a <*> Rightt b = Rightt b
  Rightt b <*> Leftt a = Rightt b
  Rightt b <*> Rightt b' = Rightt (b <> b')

instance Monoid b => Monad (PhbtEither b) where
  return = pure
  (Leftt a) >>= f = f a
  (Rightt b) >>= _ = Rightt b

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhbtEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(Leftt a), (Rightt b)]

  -- equivalent.  do notation compiles into this
  -- arbitrary =
  --   arbitrary >>= (\a ->
  --     arbitrary >>= (\b ->
  --       elements [(Leftt a), (Rightt b)]
  --                   )
  --                 )

instance (Eq a, Eq b) => EqProp (PhbtEither b a) where (=-=) = eq

main2 = do
  let trigger = undefined :: PhbtEither ((Sum Int), String, (Sum Int)) ((Sum Int), String, (Sum Int))
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger


-- 3


data Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative (Identity) where
  pure = Identity
  (Identity x) <*> (Identity x') = Identity (x x')

instance Monad Identity where
  return = pure
  (Identity x) >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

main3 = do
  let trigger = undefined :: Identity ((Sum Int), String, (Sum Int))
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

-- 4

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Foldable List where
  foldr func b Nil         = b
  foldr func b (Cons a as) = foldr func (func a b) as


-- from Tony Morris' FP course
flatten :: (List (List a)) -> (List a)
flatten lla = foldl concat' Nil lla

instance Functor List where
  fmap _ (Nil)          = Nil
  fmap f (Cons x lista) = Cons (f x) (fmap f lista)

instance Applicative (List) where
  pure x = Cons x Nil
  -- Nil <*> Nil = Nil
  -- Nil <*> (Cons y listy) = Nil
  -- (Cons x listx) <*> Nil = Nil
  -- (Cons f listf) <*> (Cons y listb) = Cons (f y) ((Cons f listf) <*> listb)
  lf <*> la = flatten $ fmap (\f -> fmap f la) lf
-- join :: Monad m => m (m a) -> m a
-- xs >>= f             = [y | x <- xs, y <- f x]

-- need to fix applicative instance for list

instance Monad List where
  return = pure
  -- (>>=) :: (List a) -> (a -> List b) -> List b
  (Nil) >>= _ = Nil
  (Cons x lista) >>= f = concat' (f x) (lista >>= f)

concat' :: (List a) -> (List a) -> List a
concat' Nil Nil                = Nil
concat' (Nil) (Cons b listb)   = Cons b listb
concat' (Cons a lista) (Nil)   = Cons a lista
concat' (Cons a lista) (listb) = Cons a (concat' lista listb)

changeList :: [a] -> List a
changeList []     = Nil
changeList (x:xs) = Cons x (changeList xs)
-- f x = Cons (f x) Nil

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
--    elements [(Cons x Nil), (Nil)]
    elements [(Cons x (Cons y Nil)), (Cons x Nil), Nil]

instance (Eq a) => EqProp (List a) where (=-=) = eq

main4 = do
  let trigger = undefined :: List ((Sum Int), String, (Sum Int))
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

-- failed applicative identity, composition, functor
-- identity - pure id <*> v = v

-- composition - pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

-- functor


--

-- 1

j :: Monad m => m (m a) -> m a
j = join

-- 2

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = fmap f ma

-- 3

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = f <$> ma <*> mb

-- 4

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf <*> ma

-- 5

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = pure []
meh (x:xs) f = do
  b <- f x
  lb <- meh xs f
  pure (b : lb)

-- 6

flipType :: (Monad m) => [m c] -> m [c]
flipType lma = meh lma id

