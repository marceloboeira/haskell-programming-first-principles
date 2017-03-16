
-- Exercises: Be Kind

-- 1
-- a -> a
-- :k a = *

-- 2
-- a -> b a -> T (b a)
-- :k b = * -> *, :k T = * -> *

-- 3
-- c a b -> c b a
-- :k c = * -> * -> *

-- Exercises: Heavy Lifting

-- 1

a = fmap (+1) $ read "[1]" :: [Int]

-- 2

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3

c = fmap (*2) (\x -> x - 2)

-- 4

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

-- 5


e :: Integer
e = let ioi = read "1" :: Integer
        lioi = [(show ioi)] :: [String]
        prepended = fmap ("123"++) lioi :: [String]
        changed = read (concat prepended) :: Integer
    in (*3) changed

-- Exercise: Possibly (Maybe clone)

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

-- write a functor instance for Either clone

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)

