import           Control.Applicative
import           Data.Functor
import           Data.Functor.Constant
import           Data.Monoid


-- 17.1 Applicative

-- Monoid allows us to mash two values of the same type together
-- Functor is for function application over some structure we don't want to worry about

-- Applicative is a monoidal functor -- this allows for function application lifted over structure (like functor), but the function we're applying is also embedded in some structure

-- 17.2 Defining Applicative

-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- every type that can have an applicative instance must also have a functor instance

-- pure function embeds something into functorial (applicative) structure

-- 17.3 Functor vs Applicative
-- Applicatives are monoidal functors, so info from monoids and functors is relevant

-- fmap vs (<*>)

-- fmap :: (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b

-- fmap f x = pure f <*> x
-- pure provides a means of embedding a value of any type in the structure we're working with

-- why does using pure on a tuple only work on right value? same reason as functor
-- "The left type is part of the structure, and the structure is not transformed by the function application"

-- 17.4 Applicative functors are monoidal functors

-- ($) :: (a -> b) -> a -> b (do nothing infix)
-- (<$>) :: (a -> b) -> f a -> f b (alias for fmap)
-- (<*>) :: f (a -> b) -> f a -> f b

-- two arguments to <*> : f (a -> b) and f a

-- :: f (a -> b) -> f a -> f b

-- use mappend

-- List

-- [(*2),(*3)] <*> [4,5] = [2 * 4, 2 * 5, 3 * 4, 3 * 5] = [8, 10, 12, 15]

-- this allows us to place our function in a list... f (a -> b)

-- instance (Monoid a, Monoid b) => Monoid (a, b) where
--   mempty = (mempty, mempty)
--   (a, b) `mappend` (a', b') =
--     (a `mappend` a', b `mappend` b')

-- instance Monoid a => Applicative ((,) a) where
--   pure x = (mempty, x)
--   (u, f) <*> (v, x) =
--     (u `mapend` v, f x)

-- 17.5 Applicative in Use

f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]
g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h z = lookup z [(2, 3), (5, 6), (7, 8)]
m x = lookup x [(4, 10), (8, 13), (1, 9001)]

-- Constant - like an identity type, except this also acts like a const function

-- (<*>) :: f (a -> b) -> f a -> f b
-- (<*>) :: Constant e (a -> b) -> Constant e a -> Constant e b

-- Maybe Applicative

-- (<*>) ::     f (a -> b) ->     f a ->     f b
-- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b

-- pure :: a -> f a
-- pure :: a -> Maybe a

-- Using the Maybe Applicative

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
  then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)
newtype Age = Age String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

mkAge :: Int -> Maybe Age
mkAge y = fmap Age $ validateLength 2 (show y)

data Person = Person Name Address Age deriving (Eq, Show)

-- mkPerson :: String -> String -> Maybe Person
-- mkPerson n a =
--   case mkName n of
--     Nothing -> Nothing
--     Just n' ->
--       case mkAddress a of
--         Nothing -> Nothing
--         Just a' ->
--           Just $ Person n' a'

-- using applicative we can simplify this

mkPerson' :: String -> String -> Int -> Maybe Person
mkPerson' n a y = Person <$> mkName n <*> mkAddress a <*> mkAge y

-- this makes it much easier to add fields i.e. "age"

-- Maybe Functor and the Name Constructor

-- instance Functor Maybe where
--   fmap _ Nothing = Nothing
--   fmap f (Just a) = Just (f a)

-- instance Applicative Maybe where
--   pure = Just
--   Nothing <*> _ = Nothing
--   _ <*> Nothing = Nothing
--   Just f <*> Just a = Just (f a)

-- Maybe Applicative and Person

data Person' = Person' Name Address deriving (Eq, Show)

-- instance Applicative Maybe where
--   pure = Just
--   Nothing <*> _ = Nothing
--   _ <*> Nothing = Nothing
--   Just f <*> Just a = Just (f a)

data Cow = Cow {
      name   :: String
    , age    :: Int
    , weight :: Int
   } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              Just (Cow nammy agey weighty)

-- we can simplify this with applicative

cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' name' age' weight' =
  Cow <$> noEmpty name' <*> noNegative age' <*> noNegative weight'

cowFromString'' :: String -> Int -> Int -> Maybe Cow
cowFromString'' name' age' weight' =
  liftA3 Cow (noEmpty name')
             (noNegative age')
             (noNegative weight')

-- 17.6 Applicative Laws

-- Identity
-- pure id <*> v = v

-- Composition
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- composing funcitons first then applying and the result of applying the functions then composing should be the same

-- Homomorphism - A structure preserving map between two algebraic structures... applying function that is embedded in structure to value in structure should be the same as applying function to value without any structure
-- pure f <*> pure x = pure (f x)

-- Interchange
-- u <*> pure y = pure ($ y) <*>

-- Just (+2) <*> pure 2 = pure ($ 2) <*> Just (+ 2) = Just 4

mPure :: a -> Maybe a
mPure = pure

embed :: Num a => Maybe ((a -> b) -> b)
embed = mPure ($ 2)

mApply :: Maybe ((a -> b) -> b)
       -> Maybe  (a -> b)
       -> Maybe              b
mApply = (<*>)

myResult = pure ($ 2) `mApply` Just (+2)

-- Every applicative instance you write should obey these 4 laws--- Identity/Composition/Homomorphism/Interchange

-- 17.8 ZipList Monoid

-- the default monoid of lists in the GHC Prelude is concatenation

-- [1,2,3] <> [4,5,6] = [1,2,3] ++ [4,5,6] = [1,2,3,4,5,6]

-- with ZipList:

-- [1,2,3] <> [4,5,6] = [1<>4, 2<>5, 3<>6]

-- Zero vs Identity

-- Zero - n * 0 == 0
-- Identity - n * 1 == n

-- identity for zipList?

  -- Sum 1 `mappend` ??? -> Sum 1

instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend
