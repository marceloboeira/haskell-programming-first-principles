import           Data.Foldable
import           Data.Monoid

-- 1

sum' :: (Foldable t, Num a) => t a -> a
sum' container = foldr (+) 0 container

-- 2

product' :: (Foldable t, Num a) => t a -> a
product' container = foldr (*) 1 container

-- 3
--foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

--foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' key container = foldr (\a b -> (helper a key) || b) False container

helper :: (Eq a) => a -> a -> Bool
helper val key = val == key

-- 4

minimum' :: (Foldable t, Functor t, Ord a) => t a -> Maybe a
minimum' container = foldr (\x y -> helper' x y) Nothing (fmap Just container)

helper' :: (Ord a) => Maybe a -> Maybe a -> Maybe a
helper' (Just x) (Nothing) = Just x
helper' (Nothing) (Just y) = Just y
helper' (Just x) (Just y)
  | x < y = Just x
  | otherwise = Just y

-- noJustHelper :: (Ord a) => a -> a -> Maybe a
-- noJustHelper x y
--   | x < y = Just x
--   | otherwise = Just y

-- minimum'' :: (Foldable t, Ord a) => t a -> Maybe a
-- minimum'' container = foldr (\x y -> noJustHelper x y) mempty container

-- 5

maximum' :: (Foldable t, Functor t, Ord a) => t a -> Maybe a
maximum' container = foldr (\x y -> helper' x y) Nothing (fmap Just container)

maxHelper' :: (Ord a) => Maybe a -> Maybe a -> Maybe a
maxHelper' (Just x) (Nothing) = Just x
maxHelper' (Nothing) (Just y) = Just y
maxHelper' (Just x) (Just y)
  | x > y = Just x
  | otherwise = Just y

-- 6

null' :: (Foldable t) => t a -> Bool
null' container = foldr (\_ _ -> False) True container

-- 7

length' :: (Foldable t) => t a -> Int
length' container = foldr (\_ acc -> acc + 1) 0 container

-- 8

toList' :: (Foldable t) => t a -> [a]
toList' container = foldr (\x acc -> (x : acc)) [] container

-- 9
-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

fold' :: (Foldable t, Monoid m) => t m -> m
fold' container = foldMap (id) container

-- 10

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f container = foldr (\x acc -> (f x) <> acc) mempty container

-- 20.6 Chapter Exercises

data Identity a = Identity a deriving (Eq, Show)

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Foldable Optional where
  foldr _ z Nada    = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada    = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a

-- 1

data Constant a b = Constant a

instance Foldable (Constant a) where
  foldr f z (Constant x) = z
  foldl f z (Constant x) = z
  foldMap f (Constant x) = mempty

-- 2

data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f z (Two x y) = f y z
  foldl f z (Two x y) = f z y
  foldMap f (Two x y) = f y

-- 3

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldr f acc (Three x y z) = f z acc
  foldl f acc (Three x y z) = f acc z
  foldMap f (Three x y z) = f z

-- 4

data Three' a b = Three' a b b

instance Foldable (Three' a) where
--  foldr f acc (Three' x y z) = (f y acc) <> (f z acc)
  foldMap f (Three' x y z) = (f y) <> (f z)

-- 5

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' w x y z) = (f x) <> (f y) <> (f z)


-- Filter function

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF testFunc container = foldr (\x acc -> if (testFunc x) then (pure x <> acc) else acc) mempty container
