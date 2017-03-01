import           Data.Monoid


-- 15.1 Monoids and semigroups

-- 15.2 algebras
-- algebras can be implemented with typeclasses

-- 15.3 Monoid - binary associative operation with an identity
-- a monoid is a function that takes two arguments and follows two laws: associativity and identity.
-- Associativity : arguments can be regrouped in different orders and give the same result (ie addition)
-- Identity - there exists some value such that when we pass it as an input to function, the operation is rendered moot and the other value is returned

-- 15.4 How monoid is defined in Haskell
-- types have an instance of a typeclass

-- class Monoid m where
--   mempty :: m
--   mappend :: m -> m -> m
--   mconcat :: [m] -> m
--   mconcat = foldr mappend mempty

-- mappend is how any two values that inhabit your type can be joined together
-- mempty is the identity for the mappend function

-- 15.5 Monoid Example

-- Lists

-- mappend [1,2,3] [4,5,6]
-- [1,2,3,4,5,6]

-- Why Integer doesn't have a Monoid - none of the numeric types have a monoid
-- in math, the monoid of numbers is summation (or multiplication) (both are binary, associative, and have identity (0 and 1))
-- no monoid in haskell for this because it's ambiguous (+ or *)
-- sum and product are special types so monoid works

-- mappend 1 1 does not work
-- mappend (Sum 1) (Sum 5) does work

-- Integers form a monoid under summation and multiplication
-- Lists form a monoid under concatenation

-- Why newtype

data Server = Server String

newtype Server' = Server' String
-- newtype constrains the datatype to having a single unary data constructor, guaranteeing no additional runtime overhead in wrapping the original type (runtime representation of newtype and what it wraps are always identical)

-- use newtype for: 1) Signal intnet (only wrapper) 2) improver type safety 3)add different typeclass instances to a type that is unchanged

-- 15.7 Why bother?
-- a good abstraction to work with when you have multiple monoidal things running around in a project... understanding lets you combine monoidal operations safely
-- something is a "Monoid", meaning we can define at least one law-abiding Monoid instance for it
-- Use monoids to structure and describe common modes of processing data (API...)

-- 15.8 Laws

-- there are more possible laws to require than associativity or identity

-- left idenitity
--mappend mempty x = x

-- right idenity
--mappend x mempty = x

-- associativity
-- mappend x (mappend y z) = mappend (mappend x y) z

-- mconcat = foldr mappend mempty

-- you have these guarentees even when you don't know what monoid you are working with

-- 15.9 Different instance, same representation

-- Bool Monoids
-- two possible: monoid of ocnjunctoin and one of disjunction (All and Any)

-- Any True <> Any True
-- Any {getAny = True}
--  Any False <> Any False
-- Any {getAny = False}
--  All False <> All False
-- All {getAll = False}
--  Any True <> Any True
-- Any {getAny = True}
--  Any False <> Any False
-- Any {getAny = False}
--  All False <> All False
-- All {getAll = False}
-- 

-- Maybe Monoid (First and Last)

-- First returns the first (leftmost) non-nothing value
-- First (Just 1) <> First (Just 2)
-- First {getFirst = Just 1}

-- Last returns the last non-nothing value
-- Last (Just 1) <> Last (Just 2)
-- Last {getLast = Just 2}

-- both can handle Nothings so long as a Just exists, otherwise returns Nothing

-- 15.10 Reusing algebras by asking for algebras
-- Maybe type

-- instance Monoid b => Monoid (a -> b)

-- instance (Monoid a, Monoid b) => Monoid (a, b)
-- instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c)

data Booly a = False' | True' deriving (Eq, Show)

instance Monoid (Booly a) where
  mappend False' _    = False'
  mappend _ False'    = False'
  mappend True' True' = True'

-- in this case, we don't need a Monoid constraint for a because we're never mappeneding a values, and we are not asking for mempty of type a

-- Exercise: Optional Monoid
-- write the Monoid instace for the Maybe type renamed to Optional

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a) (Only b) = Only (mappend a b)
  mappend (Only a) _        = Only a
  mappend _ (Only a)        = Only a

-- Associativity - we can associate the arguments of operation differently but result stays the same
-- Commutativity - can reorder arguments and get same result (++ is associative but not commutitive)

evilPlus = flip (+)

-- for our purposes, Monoid abides by the law of associativity but not the law of commuutativity

-- Identity

-- 1 + 0 = 1
-- 1 * 1 = 1

-- Orphan Instances
-- typeclasses have unique pairings of the class and the instance for a particular type
-- multiple instances for a single type when orphan instances are written (avoid at all costs)

-- orphan instance when instance is defined for a datatype and typeclass, but not in the same module as either the declaration of the typeclass or they datatype
-- use newtype to avoid this
-- see ListyInstances for example of orphan instance problems (duplicates)

-- define type and typeclass in ths same module as instances
-- define you rown newtype wrapping hte original type if needed

-- a type must have a unique implementation of a typeclass in scope

-- 15.11 Madness

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with this " <>
  adj <> " wife."

-- rewrite this using mconcat

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj =
  mconcat [e, "! he said ", adv, " as he jumped into his car ", noun, " and drove off with this ", adj, " wife."]

-- 15.12 Better living through QuickCheck
-- a good way of geting a quick sense if laws are likely to be obeyed by an instance

-- validating associativity with QuickCheck

-- asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
-- asc (<>) a b c = a <> (b <> c) == (a <> b) <> c

-- monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
-- monoidAssoc a b c = (a <> (b <>c)) == ((a <> b) <> c)

-- must declare types so QuickCheck knows what types of data to generate

-- 15.13 Semigroups

-- to get from monoid to a semigroup, we no longer furnish nor require an identity (still binary and associative)

--class Semigroup a where
--  (<>) :: a -> a -> a

--(a <> b) <> c = a <> (b <> c)
-- semigroup is not yet part of base (unless GHC 8 or later)

-- NonEmpty

data NonEmpty a = a :| [a] deriving (Eq, Ord, Show)
-- or
newtype NonEmpty' a = NonEmpty (a, [a]) deriving (Eq, Ord, Show)
-- can't write a monoid for NonEmpty because it has no identity by design

data P = Prefix Int String

data Q = Int :!!: String

-- Moniod is stronger than Semigroup, it has a strict superset of the laws that Semigroup provides (Monoid superclasses semigroup)
