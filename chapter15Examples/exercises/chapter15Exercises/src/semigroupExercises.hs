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
main = quickCheck (semigroupAssoc :: BoolAssoc)

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
