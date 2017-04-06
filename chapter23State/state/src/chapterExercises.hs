{-# LANGUAGE InstanceSigs #-}

import           Control.Applicative
import           Data.Char

-- 23.8 Chapter Exercises


newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> ((f (fst (g s))), s)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi (\s -> (((fst (f s)) (fst (g s))) , s))

-- state Monad

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s0 -> let
    (a, s1) = f s0
    moisb = g a
    sbs = runMoi moisb
    (b, s2) = sbs s1
    in (b, s2)

-- 1

get :: Moi s s
get = Moi $ (\s -> (s, s))

-- 2

put :: s -> Moi s ()
put s = Moi $ (\_ -> ((), s))

-- 3

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s

-- 4

eval :: Moi s a -> s -> a
eval (Moi sa) s = fst (sa s)

-- 5

modify :: (s -> s) -> Moi s ()
modify func = Moi $ \s -> ((), func s)

incrementOne :: Moi Int ()
incrementOne = modify (\i -> i+1)

incrementOneAndLog :: Moi Int String
incrementOneAndLog = Moi $ \s -> ("the current state is "++(show s), s+1)

one = runMoi incrementOneAndLog 0

incrementThreeAndLog :: Moi Int String
incrementThreeAndLog = do
  _ <- incrementOneAndLog
  _ <- incrementOneAndLog
  incrementOneAndLog

three = runMoi incrementThreeAndLog 0

incrementThreeAndLog' :: Moi Int String
incrementThreeAndLog' = incrementOneAndLog >>= (\_ -> incrementOneAndLog >>= (\_ -> incrementOneAndLog))

three' = runMoi incrementThreeAndLog' 0
four' = runMoi incrementThreeAndLog' 1

incrementThreeAndLog'' :: Moi Int String
incrementThreeAndLog'' = incrementOneAndLog >> incrementOneAndLog >> incrementOneAndLog

three'' = runMoi incrementThreeAndLog'' 0
--   :t (>>)
-- (>>) :: Monad m => m a -> m b -> m b
--   :t (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
