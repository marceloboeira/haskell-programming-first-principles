module EitherMonad where

import           Control.Monad
import           Data.Monoid

type Founded = Int

type Coders = Int

data SoftwareShop =
  Shop {
      founded     :: Founded
    , programmers :: Coders
  } deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0     = Left $ NegativeYears n
  | n > 500   = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0     = Left $ NegativeCoders n
  | n > 5000  = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded     <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
    then Left $ TooManyCodersForYears founded programmers
    else Right $ Shop founded programmers

-- (<*>) == ap

-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- ap ::          Monad m => m (a -> b) -> m a -> m b

-- deriving applicative from the stronger instance

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap m m' = do
  x  <- m
  x' <- m'
  return (x x')

-- you cant mae a Monad for validation that accumulates the errors like Applicative does

-- Exercise: implement the Either Monad
--(>>=) :: m a -> (a -> m b) -> m b

data Sum' a b = First' a | Second' b deriving (Eq, Show)

instance Functor (Sum' a) where
  fmap f (First' x)  = First' x
  fmap f (Second' y) = Second' (f y)

instance Monoid a => Applicative (Sum' a) where
  pure x = Second' x
  (First' x) <*> (First' x') = (First' (x <> x'))
  (First' x) <*> (Second' y) = (First' x)
  (Second' f) <*> (First' x) = (First' x)
  (Second' f) <*> (Second' y) = (Second' (f y))

instance Monoid a => Monad (Sum' a) where
  return = pure
  (First' x) >>= f = First' x
  (Second' y) >>= f = f y



