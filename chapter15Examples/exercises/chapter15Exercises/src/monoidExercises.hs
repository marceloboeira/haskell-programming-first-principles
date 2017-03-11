
import           Data.Semigroup
import           Test.QuickCheck

-- given a datatype, implement the monoid instance

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _  = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

instance Semigroup Int where
  a <> b = a + b

instance Monoid Int where
  mempty = 0
  mappend a b = a + b

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: BooljAssoc)
  quickCheck (monoidLeftIdentity :: MonoBoolj)
  quickCheck (monoidRightIdentity :: MonoBoolj)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = mappend a mempty == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = mappend mempty a == a

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- 2

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance (Monoid a, Num a) => Monoid (Identity a) where
  mempty = Identity 0
  mappend (Identity x) (Identity y) = Identity (mappend x y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

type IdAssoc a = (Identity a) -> (Identity a) -> (Identity a) -> Bool
type MonoId a = (Identity a) -> Bool

-- 3


data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Monoid a, Monoid b, Num a, Num b) => Monoid (Two a b) where
  mempty = Two 0 0
  mappend (Two a b) (Two c d) = Two (mappend a c) (mappend b d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

type TwoAssoc a b = (Two a b) -> (Two a b) -> (Two a b) -> Bool
type MonoTwo a b = Two a b -> Bool

-- 4

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Monoid Bool where
  mappend True True = True
  mappend _ _       = False

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend (BoolConj a) (BoolConj b) = BoolConj (mappend a b)

instance Arbitrary BoolConj where
  arbitrary = boolGen

boolGen :: Gen BoolConj
boolGen = elements [(BoolConj True), (BoolConj False)]

type BoolAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type MonoBool = BoolConj -> Bool

-- 5

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend (BoolDisj False) (BoolDisj False) = BoolDisj False
  mappend _ _                               = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = booljGen

booljGen :: Gen BoolDisj
booljGen = elements [(BoolDisj True), (BoolDisj False)]

type BooljAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type MonoBoolj = BoolDisj -> Bool

-- 6


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

-- 7

newtype Comp a = Comp { unComp :: (a -> a) }

instance (Semigroup a) => Semigroup (Comp a) where
  (Comp f1) <> (Comp f2) = Comp (f1 <> f2)

-- instance (Arbitrary a) => Arbitrary (Comp a) where
--   arbitrary = compGen

-- compGen :: (Arbitrary a) => Gen (Comp a)
-- compGen = do
--   a <- arbitrary
--   return (Comp a)

-- 8

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance (Num s, Monoid a) => Monoid (Mem s a) where
  mempty = Mem (\a -> [], a + 0)
  mappend = undefined


f' = Mem $ \s -> ("hi", s + 1)

main' = do
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0

