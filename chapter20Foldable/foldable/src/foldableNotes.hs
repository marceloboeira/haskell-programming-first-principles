
--import           Data.Foldable
import           Data.Monoid

-- 20.1 Foldable - always dependent on some Monoid instance

-- 20.2 The Foldable class - a class of data structures that can be folded to a summary value

-- 20.3 Revenge of the Monoids
-- folding necessarily implies a binary associative operation that has an identity value

-- class Foldable t where
--   {-# MINIMAL foldMap | foldr #-}

-- -- class Foldable (t :: * -> *) where
--   fold :: Data.Monoid.Monoid m => t m -> m
--   foldMap :: Data.Monoid.Monoid m => (a -> m) -> t a -> m

-- 20.4 Demonstrating Foldable instances

-- Identity

data Identity a = Identity a deriving (Eq, Show)

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

-- Maybe

-- foldr (+) 1 Nothing = 1

-- foldMap (+1) Nothing :: Sum Integer = Sum {getSum = 0}

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Foldable Optional where
  foldr _ z Nada    = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada    = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a

-- 20.5 Some basic derived operations

-- toList :: t a -> [a]


