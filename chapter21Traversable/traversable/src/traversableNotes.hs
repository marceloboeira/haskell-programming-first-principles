import           Data.Maybe
import           Data.Traversable
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
-- 21.1 Traversable

-- functor transforms any values embedded in structure
-- applicative is a monoidal functor, gives us a way to transform values contained in structure with functions contained in structure
-- foldable gives us a way to process values embedded in a structure as if they existed in sequential order

-- traversable depends on Applicative, and thus functor, superclassed by foldable

-- traversable allows you to transform elements inside the structure (like functor), producing applicative effects along the way, and lift instances of Applicative structure outside of the Traversable structure

-- 21.2 Traversable typeclass definition

-- class (Functor t, Foldable t) => Traversable t where
--     {-# MINIMAL traverse | sequenceA #-}
--     traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--     traverse f = sequenceA . fmap f

--     sequenceA :: Applicative f => t (f a) -> f (t a)
--     sequenceA = traverse id

-- 21.3 sequenceA

-- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)

-- 21.4 traverse

-- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)

-- 21.5 What's traversable for? - flipping type constructors, or mapping something then flip

-- 21.6 Morse code revisited

-- stringToMorse :: String -> Maybe [Morse]
-- stringToMorse s = sequence $ fmap charToMorse s

--- what we want

-- stringToMorse :: String -> Maybe [Morse]
-- stringToMorse = traverse charToMorse

-- sequence :: (Monad m, Traversable t) => t (m a) -> m (t a)

-- traverse is just fmap and the Traversable version of sequence bolted together into one convenient functions

-- 21.7 Axing tedious code

data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

-- before

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left $ err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a

-- clean this up

pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (mapM decodeFn a)

-- or

pipelineFn'' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn'' = (traverse makeIoOnlyObj . mapM decodeFn =<<) . fetchFn


-- 21.8 Do all the things

-- Traversable is stronger than Functor and Foldable


-- 21.9 Traversable instances


-- Either

data Either' a b = Left' a | Right' b deriving (Eq, Ord, Show)

instance Functor (Either' a) where
  fmap _ (Left' x)  = Left' x
  fmap f (Right' y) = Right' (f y)

instance Applicative (Either' e) where
  pure = Right'
  Left' e <*> _ = Left' e
  Right' f <*> r = fmap f r

instance Foldable (Either' a) where
  foldMap _ (Left' _)  = mempty
  foldMap f (Right' y) = f y

  foldr _ z (Left' _)  = z
  foldr f z (Right' y) = f y z

instance Traversable (Either' a) where
  traverse _ (Left' x)  = pure (Left' x)
  traverse f (Right' y) = Right' <$> f y

-- Tuple

-- instance Functor ((,) a) where
--   fmap f (x,y) = (x, f y)

-- instance Monoid a => Applicative ((,) a) where
--   pure x = (mempty, x)
--   (u, f) <*> (v, x) = (u `mappend` v, f x)

-- instance Foldable ((,) a) where
--   foldMap f (_, y) = f y
--   foldr f z (_, y) = f y z

-- instance Traversable ((,) a) where
--   traverse f (x, y) = (,) x <$> f y

-- 21.10 Traversable Laws

-- 1 Naturality - t . traverse f = traverse (t . f)
-- 2 Identity - traverse Identity = Identity
-- 3 Composition - traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f

-- 21.11 Quality Control

type TI = []

main = do
  let trigger = undefined :: TI (Int, Int, [Int])
  quickBatch (traversable trigger)
