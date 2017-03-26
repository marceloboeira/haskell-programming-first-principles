import           Control.Applicative
import           Control.Monad

-- 18.1 Monad
-- Monads are defined in terms of Haskell
-- Monads are applicative functors

-- 18.2 Monad is not a burrito

-- functor maps a function over structure
-- applicative maps a function that is contained in structure over some other structure then combines the two layers of structure like mappend

-- monads are another way of applying functions over structure

-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> mb
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a

-- applicative m (Monad > Applicative > Functor)
-- can derive applicative and functor in term of Monad

-- fmap f xs = xs >>= return . f
-- Functor -> Applicative -> Monad
-- whenever you implement a Monad instance you will always have an Applicative and Functor too

-- Core operations

-- (>>=) :: m a -> (a -> m b) -> m b
  -- (Bind - contains the special things about Monad)
-- (>>) :: m a -> m b -> m b
  -- (Mr Pointy -- sequences two actions while discarding and resulting value from the first action)
-- return :: a -> m a (same thing as pure)

-- the novel part of Monad

-- type of >>= is similar to fmap and apply

-- fmap :: Functor f     =>   (a -> b) -> f a ->        f b
-- <*>  :: Applicative f => f (a -> b) -> f a ->        f b
-- >>=  :: Monad f       => f a ->        (a -> f b) -> f b

-- in ways, Monad is a generalization of concat ... concat :: [[a]] -> [a]

-- (join :: Monad m => m (m a) -> m a) vs (concat :: [[a]] -> [a])

-- we can inject more structure via our function application, whereas applicatives and fmaps have to leave the structure untouched

-- write bind in terms of fmap and join

bind :: Monad m => (a -> m b) -> m a -> m b
bind f xs = join  (fmap f xs)

-- What Monad is not
-- Impure... Monadic functions are pure functions
-- an embedded language for imperative programming
-- A value
-- about strictness

-- Monad lifts

-- liftA :: Applicative f => (a -> b)  -> f a  -> f b
-- liftM :: Monad m =>       (a1 -> r) -> m a1 -> m r

-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r

-- 18.3 Do syntax and monads -- works with any monad, but is most commonly seen with IO

-- (*>) :: Applicative f => f a -> f b -> f b
-- (>>) :: Monad m =>       m a -> m b -> m b

sequencing :: IO ()
sequencing = do
  putStrLn "Blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
  putStrLn "Blah" >>
  putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "Blah" *>
  putStrLn "another thing"

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' =
  getLine >>= putStrLn

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>=
  \name -> putStrLn ("y helo thar: " ++ name)

twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y helo thar: "
           ++ name ++ " who is: "
           ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:" >>
  getLine >>=
  \name ->
  putStrLn "age pls:" >>
  getLine >>=
  \age ->
  putStrLn ("y hello there: "
           ++ name ++ " who is: "
           ++ age ++ " years old.")


-- 18.4 Examples of Monad Use

-- list

-- (>>=) :: Monad m => m  a -> (a ->  m  b) -> m  b
-- (>>=) ::           [ ] a -> (a -> [ ] b) ->[ ] b

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

-- Maybe

-- (>>=) :: Monad m => m  a -> (a ->     m b) ->     m b
-- (>>=) ::         Maybe a -> (a -> Maybe b) -> Maybe b

return :: Monad m => a ->     m a
return ::            a -> Maybe a

data Cow = Cow {
      name :: String
      
