import           Control.Monad
import           Data.Monoid
import           Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

-- main :: IO ()
-- main = do
--   quickCheck (monoidAssoc :: BullMappend)
--   quickCheck (monoidLeftIdentity :: Bull -> Bool)
--   quickCheck (monoidRightIdentity :: Bull -> Bool)

data Optional a = Nada | Only a deriving (Eq, Show)

newtype First' a =
  First' { getFirst' :: Optional a } deriving (Eq, Show)

-- instance (Monoid a) => Monoid (First' a) where
--   mempty = First' Nada
--   mappend (First' (Only a)) (First' (Only b)) = First' (Only (mappend a b))
--   mappend (First' (Only a)) _                 = First' (Only a)
--   mappend _ (First' (Only a))                 = First' (Only a)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' (Only a)) (First' (Only b)) = First' (Only a)
  mappend (First' (Only a)) _                 = First' (Only a)
  mappend _ (First' (Only a))                 = First' (Only a)
  mappend _ _                                 = First' Nada

firstMappend :: (Monoid a) => First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

-- main :: IO ()
-- main = do
--   quickCheck (monoidAssoc :: FirstMappend)
--   quickCheck (monoidLeftIdentity :: First' String -> Bool)
--   quickCheck (monoidRightIdentity :: First' String -> Bool)

