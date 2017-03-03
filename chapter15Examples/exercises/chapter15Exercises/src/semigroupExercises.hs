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
main = quickCheck (semigroupAssoc :: IdAssoc Int)

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

-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
--   arbitrary = fmap Two (arbitrary arbitrary)

-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
--   arbitrary = oneof [liftM Left arbitrary, liftM Right arbitrary]
--   shrink (Left x)  = [ Left  x' | x' <- shrink x ]
--   shrink (Right y) = [ Right y' | y' <- shrink y ]
