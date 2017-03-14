import           Test.QuickCheck
import           Test.QuickCheck.Function


-- 16.9 QuickChecking Functor instances
-- fmap id = id
-- fmap (p . q) = fmap p . fmap q

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- QuickCheck -- CoArbitrary used to generate functions

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

-- 16.10 Exercises: Instances of Func

-- 1

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

type IdFC = (Identity Int) -> IntToInt -> IntToInt -> Bool

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

-- identityGenInt :: Gen (Identity Int)
-- identityGenInt = identityGen

-- 2

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

type PairFC = (Pair Int) -> IntToInt -> IntToInt -> Bool

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = pairGen

pairGen :: Arbitrary a => Gen (Pair a)
pairGen = do
  x <- arbitrary
  y <- arbitrary
  return (Pair x y)

-- 3

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two (x) (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  x <- arbitrary
  y <- arbitrary
  return (Two x y)

type TwoFC = (Two Int Int) -> IntToInt -> IntToInt -> Bool

-- 4

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three (x) (y) (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return (Three x y z)

type ThreeFC = (Three Int Int Int) -> IntToInt -> IntToInt -> Bool

-- 5

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' (x) (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = three'Gen

three'Gen :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
three'Gen = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return (Three' x y z)

type Three'FC = (Three Int Int Int) -> IntToInt -> IntToInt -> Bool

-- 6

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = fourGen

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

type FourFC = (Four Int Int Int Int) -> IntToInt -> IntToInt -> Bool

-- 7

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = four'Gen

four'Gen :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
four'Gen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four' a b c d)

type Four'FC = (Four Int Int Int Int) -> IntToInt -> IntToInt -> Bool

-- 8

data Trivial = Trivial deriving (Eq, Show)

-- instance Functor Trivial where
--   fmap f Trivial = Trivial
-- cannot write an instance for trivial because it is already fully concrete (*)

