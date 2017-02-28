module Listy where

import           Data.Monoid

newtype Listy a =
  Listy [a]
  deriving (Eq, Show)

instance Monoid (Listy a) where
  mempty = Listy []
  mappend (Listy l) (Listy l') = Listy $ mappend l l'

-- causes error when this Monoid instance is included, is amgibuous with ListyInstances instance
