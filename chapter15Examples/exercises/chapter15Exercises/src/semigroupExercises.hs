import           Data.Semigroup
import           Test.QuickCheck

-- 1

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Semigroup Int where
  x <> y = x+y

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main = quickCheck (semigroupAssoc :: ValAssoc Int Int)

-- 2

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary
--  arbitrary = return (Identity (arbitrary))

-- :t arbitrary :: Gen Int
-- arbitrary :: Gen Int :: Gen Int
-- :t fmap (\i -> show i) (arbitrary :: Gen Int)
-- fmap (\i -> show i) (arbitrary :: Gen Int) :: Gen String
-- :t fmap (Identity) (arbitrary :: Gen Int)
-- fmap (Identity) (arbitrary :: Gen Int) :: Gen (Identity Int)

type IdAssoc a = (Identity a) -> (Identity a) -> (Identity a) -> Bool

-- 3

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

-- twoGenIntString :: Gen (Two Int String)
-- twoGenIntString = twoGen

type TwoAssoc a b = (Two a b) -> (Two a b) -> (Two a b) -> Bool


-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
--   arbitrary = oneof [liftM Left arbitrary, liftM Right arbitrary]
--   shrink (Left x)  = [ Left  x' | x' <- shrink x ]
--   shrink (Right y) = [ Right y' | y' <- shrink y ]

-- 4

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three d e f) = Three (a <> d) (b <> e) (c <> f)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

type ThreeAssoc a b c = (Three a b c) -> (Three a b c) -> (Three a b c) -> Bool

-- 5

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four e f g h) = Four (a <> e) (b <> f) (c <> g) (d <> h)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = fourGen

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

type FourAssoc a b c d = (Four a b c d) -> (Four a b c d) -> (Four a b c d) -> Bool

-- 6

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = boolGen

boolGen :: Gen BoolConj
boolGen = elements [(BoolConj True), (BoolConj False)]

type BoolAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = booljGen

booljGen :: Gen BoolDisj
booljGen = elements [(BoolDisj True), (BoolDisj False)]

type BooljAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  (Snd b) <> _ = Snd b
  (Fst a) <> (Snd b) = Snd b
  (Fst a) <> (Fst b) = Fst b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = orGen

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do
  a <- arbitrary
  b <- arbitrary
  elements [(Fst a), (Snd b)]

type OrAssoc a b = (Or a b) -> (Or a b) -> (Or a b) -> Bool

-- 9

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
  (Combine f1) <> (Combine f2) = Combine (f1 <> f2)

-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
--   arbitrary = combGen

-- combGen :: (Arbitrary b) => Gen (Combine a b)
-- combGen = do
--   b <- arbitrary
--   return (Combine b)

-- figure our CoArbitrary to quickCheck this

-- 10

newtype Comp a = Comp { unComp :: (a -> a) }

instance (Semigroup a) => Semigroup (Comp a) where
  (Comp f1) <> (Comp f2) = Comp (f1 <> f2)

-- instance (Arbitrary a) => Arbitrary (Comp a) where
--   arbitrary = compGen

-- compGen :: (Arbitrary a) => Gen (Comp a)
-- compGen = do
--   a <- arbitrary
--   return (Comp a)

-- 11

data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Validation a b) where
  (Failure' a) <> (Failure' b) = Failure' (a <> b)
  (Failure' a) <> _ = Failure' a
  _ <> (Failure' b) = Failure' b
  (Success' a) <> (Success' b) = Success' (a <> b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = valGen

valGen :: (Arbitrary a, Arbitrary b) => Gen (Validation a b)
valGen = do
  a <- arbitrary
  b <- arbitrary
  elements [(Failure' a), (Success' b)]

type ValAssoc a b = (Validation a b) -> (Validation a b) -> (Validation a b) -> Bool


-- 12

newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where

