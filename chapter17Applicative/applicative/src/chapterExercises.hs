import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
-- 1

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

-- pure id <*> x = x

instance Applicative (Pair) where
  pure a = Pair a a
  Pair f f' <*> Pair a a' = Pair (f a) (f' a')

instance Monoid a => Monoid (Pair a) where
  mempty = Pair mempty mempty
  mappend (Pair a a') (Pair b b') = Pair (a <> b) (a' <> b')


-- checkPairMonoid :: IO ()
-- checkPairMonoid = quickBatch

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = pairGen

pairGen :: Arbitrary a => Gen (Pair a)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance Eq a => EqProp (Pair a) where (=-=) = eq


-- y
-- 3
-- u
-- Pair <function> <function>
-- pure ($ y) <*> u
-- Pair 4 5
-- u <*> (pure y)
-- Pair 4 5

-- 2

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  -- wrong
  -- (Two (a -> b) (c -> d)) -> (Two e c) -> (Two e d)
  -- Fixed: changed Two (a) (g b) to Two (f <> a) (g b), passes interchange
  Two f g <*> Two a b = Two (f <> a) (g b)


instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two x y) (Two x' y') = Two (x <> x') (y <> y')

-- checkTwoMonoid :: IO ()
-- checkTwoMonoid = quickBatch (monoid Two Int)

-- instance Applicative (Two a) where
--   -- pure :: Applicative b => a -> Two a b
--   pure x = Two x (pure x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  x <- arbitrary
  y <- arbitrary
  return (Two x y)

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

-- checkTwo :: IO ()
-- checkTwo = quickBatch (applicative Two)

-- Interchange Law
-- u <*> pure y = pure ($ y) <*> u


----------------

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  mappend (Three x y z) (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure z = Three mempty mempty z
  Three f f' f'' <*> Three x y z = Three (f <> x) (f' <> y) (f'' z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 (\x y z -> Three x y z) arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

-- 4

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Monoid a, Monoid b) => Monoid (Three' a b) where
  mempty = Three' mempty mempty mempty
  mappend (Three' x y z) (Three' x' y' z') = Three' (x <> x') (y <> y') (z <> z')

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  Three' f f' f'' <*> Three' x y z = Three' (f <> x) (f' y) (f'' z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 (\x y z -> Three' x y z) arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

-- 5

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty
  mappend (Four w x y z) (Four w' x' y' z') = Four (w<>w') (x<>x') (y<>y') (z<>z')

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  Four f f' f'' f''' <*> Four w x y z = Four (f <> w) (f' <> x) (f'' <> y) (f''' z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
  arbitrary = (\w x y z -> Four w x y z) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq

-- 6

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)

instance (Monoid a, Monoid b) => Monoid (Four' a b) where
  mempty = Four' mempty mempty mempty mempty
  mappend (Four' w x y z) (Four' w' x' y' z') = Four' (w<>w') (x<>x') (y<>y') (z<>z')

instance (Monoid a) => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  Four' f f' f'' f''' <*> Four' w x y z = Four' (f <> w) (f' <> x) (f'' <> y) (f''' z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = (\w x y z -> Four' w x y z) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq


------ Combinations



stops :: [Char]
stops = "pbtdkg"

vowels :: [Char]
vowels = "aeiou"

ends :: [Char]
ends = "pbtdkg"

charToString :: Char -> String
charToString a = [a]

combos :: [(Char, Char, Char)]
combos = (\a b c -> (a, b, c)) <$> stops <*> vowels <*> stops

combos' :: [a] -> [b] -> [c] -> [(a, b, c)]
combos' s v s' = liftA3 (\a b c -> (a, b, c)) s v s'
