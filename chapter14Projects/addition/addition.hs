module Addition where

import           Test.Hspec
import           Test.QuickCheck

-- sayHello :: IO ()
-- sayHello = putStrLn "hello!"

-- main :: IO ()
-- main = hspec $ do
--   describe "Addition" $ do
--     it "1 + 1 is greater than 1" $ do
--       ((1 :: Int) + (1 :: Int)) > (1 :: Int) `shouldBe` True

-- shouldBe :: (Eq a, Show a) => a -> a -> Expectation
-- compare to
-- (==) :: Eq a => a -> a -> Bool

-- main :: IO ()
-- main = hspec $ do
--   describe "Addition" $ do
--     it "1 + 1 is greater than 1" $ do
--       ((1::Int) + (1::Int)) > (1::Int) `shouldBe` True
--     it "2 + 2 is equal to 4" $ do
--       (2::Int) + (2::Int) `shouldBe` (4::Int)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

-- main :: IO ()
-- main = hspec $ do
--   describe "Addition" $ do
--     it "15 divided by 3 is 5" $ do
--       dividedBy (15 :: Int) (3 :: Int) `shouldBe` ((5 :: Int), (0 :: Int))
--     it "22 divided by 5 is 4 remainder 2" $ do
--       dividedBy (22 :: Int) (5 :: Int) `shouldBe` ((4 :: Int), (2 :: Int))

-- Intermission

testMult :: (Eq a, Num a) => a -> a -> a
testMult a 0 = 0
testMult a b = a + testMult a (b - 1)

-- main :: IO ()
-- main = hspec $ do
--   describe "Muliplication by sum" $ do
--     it "13 times 10 is 130" $ do
--       testMult (13 :: Int) (10 :: Int) `shouldBe` (130 :: Int)

-- Hspec is limited to particular values

-- using QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy (15 :: Int) (3 :: Int) `shouldBe` ((5 :: Int), (0 :: Int))
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy (22 :: Int) (5 :: Int) `shouldBe` ((4 :: Int), (2 :: Int))
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

-- Arbitrary Instances - arbitrary is a value of type Gen


trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,2,2,2,2,3]
-- only drawing randomly from this pool... stacked odds toward 2

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

-- generators with polymorphic type arguments

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

--

genEither :: (Arbitrary a,  Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [(1, return Nothing)
             ,(3, return (Just a))]

-- Using QuickCheck withough Hspec

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

-- 14.5 Morse Code
-- see new project
