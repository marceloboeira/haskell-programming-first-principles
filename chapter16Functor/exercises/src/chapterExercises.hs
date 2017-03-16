{-# LANGUAGE FlexibleInstances #-}

import           GHC.Arr

-- 16.17 Chapter Exercises

-- determine if a valid functor can be written

-- 1
data Bool = False | True

-- no valid Functor because there is no contained type to act on

-- 2


data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a)  = True' (f a)

-- 3

data BoolAndMaybeSomethingElse a = Falsish | Truish a deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish    = Falsish
  fmap f (Truish a) = Truish (f a)

-- 4

-- newtype Mu f = InF { outF :: f (Mu f)}

-- instance Functor (Mu) where
--   fmap _ (InF outF) = InF (outF)
-- Not possible due to Mu's kind

-- 5

data D = D (Array Word Word) Int Int

-- cannot write functor instance as D has kind *

-- more Functor Instances

-- 1

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum e) where
  fmap _ (First a)  = First (a)
  fmap f (Second b) = Second (f b)

-- 2

data Company a b c = DeepBlue a c | Something b deriving (Eq, Show)

instance Functor (Company e e') where
  fmap _ (Something b)  = Something b
  fmap f (DeepBlue a c) = DeepBlue (a) (f c)

-- 3

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- write functor instances

-- 1

data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- 2

--data K b a = K a deriving (Eq, Show)

--instance Functor (K b) where
--  fmap f (K a) = K (f a)

-- 3

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap f (K x) = K x

-- instance Functor (Flip K a) where
--   fmap fn (Flip ((K a) b a)) = Flip (K (fn b) a)

-- Flip only has one argument (f b a)

instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip (K (f x))

-- 4

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

newtype F a = F a deriving (Eq, Show)

instance Functor (LiftItOut F) where
  fmap f (LiftItOut (F a)) = LiftItOut (F (f a))

-- 6

data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

newtype G a = G a deriving (Eq, Show)

instance Functor (Parappa F G) where
  fmap f (DaWrappa (F a) (G b)) = DaWrappa (F (f a)) (G (f b))

-- 7

data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance Functor (IgnoreOne F G a) where
 fmap f (IgnoringSomething (F a) (G b)) = IgnoringSomething (F a) (G (f b))

-- 8

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor (Notorious G o a) where
  fmap f (Notorious (G o) (G a) (G t)) = Notorious (G o) (G a) (G (f t))

-- 9

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor (List) where
  fmap _ Nil             = Nil
  fmap f (Cons x (list)) = Cons (f x) (fmap f list)

-- 10

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Eq, Show)

instance Functor (GoatLord) where
   fmap _ NoGoat = NoGoat
   fmap f (OneGoat a) = OneGoat (f a)
   fmap f (MoreGoats tree1 tree2 tree3)
    = MoreGoats (fmap f tree1) (fmap f tree2) (fmap f tree3)

-- 11

data TalkToMe a = Halt | Print String a | Read (String -> a)

-- instance Functor (Read (String -> a)) where
--    fmap f (fx) = f (fx)

instance Functor (TalkToMe) where
   fmap _ Halt             = Halt
   fmap f (Print string a) = Print string (f a)
   fmap f (Read stringa)   = Read (fmap f stringa)

foo :: Int -> String
foo = show

-- functions have a functor instance
-- we are fmapping over the function foo

bar :: Int -> String
bar = fmap (\s -> s ++ s) foo

bar' :: Int -> String
bar' = (\s -> s ++ s) . foo

bar'' :: Int -> String
bar'' n = (\s -> s ++ s) (foo n)

