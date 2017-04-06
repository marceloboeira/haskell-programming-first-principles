{-# LANGUAGE InstanceSigs #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State
import           System.Random

-- 23.1 State - state is data that exists in addition to the inputs and outputs of our functions

-- 23.2 What is state

-- simplest form of state is a light switch
-- state is implicit in imperative programming languages, not haskell
-- in place mutation with the ST type

-- 23.3 Random Numbers

-- system.Random generates pseudorandom values

-- mkStdGen :: Int -> StdGen
-- next :: g -> (Int, g)
-- g is of type StdGen
-- random :: (RandomGen g, Random a) => g -> (a, g)

-- 23.4 The State newtype

--newtype State s a = State { runState :: s -> (a, s) }

-- isomorphism

type Iso a b = (a -> b, b -> a)
newtype Sum a = Sum { getSum :: a }

sumIsIsomorphicWithItsContents :: Iso a (Sum a)
sumIsIsomorphicWithItsContents = (Sum, getSum)

-- not isomorphic:
-- (a -> Maybe b, b -> Maybe a) - could lose information

-- state is a function that takes input state and returns an output value a

-- random :: (Random a) => StdGen -> (a, StdGen)
-- State {   runState ::        s -> (a, s)    }

-- 23.5 Throw Down

data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

-- this will produce the same results every time, because it is free of effects
-- we can improve this with State

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

-- state :: Monad m => (s -> (a, s)) -> StateT s m a

rollDie' :: State StdGen Die
rollDie' =
  intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 20 = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen

-- Exercises: Roll your own

-- 1

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= n = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen

-- 2

rollsLogged :: Int -> StdGen -> (Int, [Int])
rollsLogged n g = go 0 0 g []
  where go :: Int -> Int -> StdGen -> [Int] -> (Int, [Int])
        go sum count gen dies
          | sum >= n = (count, dies)
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen (die : dies)

-- 23.6 -- Write State for yourself

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> ((f (fst (g s))), s)

-- instance Functor (Reader r) where
--   fmap f (Reader ra) = Reader (f . ra)

-- instance Applicative (Reader r) where
--   pure :: a -> Reader r a
--   pure a = Reader $ (\_ -> a)
--   (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
--   (Reader rab) <*> (Reader ra) = Reader $ rab <*> ra

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
--   (s -> ((a -> b), s))-(s -> (a, s))-(s -> (b, s))
  (Moi f) <*> (Moi g) = Moi (\s -> (((fst (f s)) (fst (g s))) , s))
--  (Moi f) <*> (Moi g) = Moi (\s -> ((f s) <*> (g s), s))

-- state Monad

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
--      (s -> (a, s)) (a -> (s -> (a, s)))  (s -> (a, s))
  -- do not return s -> Moi s b
  -- do not return Moi (Moi s b) b
  -- return Moi s b
--  (Moi f) >>= g = Moi $ (\s -> $ g (fst (f s)))
--  (Moi f) >>= g = g . fst . f
  (Moi f) >>= g = Moi $ \s0 -> let
    (a, s1) = f s0
    moisb = g a
    sbs = runMoi moisb
    (b, s2) = sbs s1
    in (b, s2)


-- increment = Moi $ \s -> ("state is now "++(show s+1), s+1)

-- increment = Moi $ \s -> ((), s+1)
-- made with `modify`
-- increment = modify (\i -> i + 1)
--              = moi $ \s -> ((), s+1)
-- increment3 = do
--   first <- increment
--   second <- increment
--   third <- intrement
--   return third
--
-- runMoi increment3 0  = (3, 3)
-- runMoi increment3 6 = (6, 6)

-- 23.7 -- fizzbuzz

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

fizzMain :: IO ()
fizzMain = mapM_ (putStrLn . fizzBuzz) [1..100]

fizzbuzzList :: [Integer] -> [Integer]
fizzbuzzList list =
  execState (mapM_ addResult list) []

-- addResult :: Integer -> State [String]
-- addResult n = do
--     xs <- get
--     let result = fizzBuzz n
--     put (result : xs)
