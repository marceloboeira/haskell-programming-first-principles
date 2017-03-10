module Main where

import           GHC.Generics
import           Test.QuickCheck

data Trivial =
  Trivial
  deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

main :: IO ()
main = do
  sample trivialGen

data Identity a =
  Identity a
  deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

-- greater than the sum of its parts

data Sum a b = First a | Second b deriving (Eq, Show)

-- sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
-- sumGenEqual = do
--   a <- arbitrary
--   b <- arbitrary
--   oneof [return $ First a,
--          return $ Second b]

-- use frequency instead of oneof to effect probability

sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  frequency [(1,return $ First a),
             (5, return $ Second b)]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

-- CoArbitrary - lets you provide functions with a value of type a as an argument in order to vary a Gen

-- arbitrary :: Arbitrary a => Gen a

-- coarbitrary :: CoArbitrary a => a -> Gen b -> Gen b

--data Bool' = True' | False' deriving (Generic)

--instance CoArbitrary Bool

trueGen :: Gen Int
trueGen = coarbitrary True arbitrary

falseGen :: Gen Int
falseGen = coarbitrary False arbitrary
