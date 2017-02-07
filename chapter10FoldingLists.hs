import           Data.List

-- 10.1 Folds (catamorphisms)
-- a means of deconstructing data

-- 10.2 Bringing you into the fold

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

listFoldr = foldr :: (a -> b -> b) -> b -> [] a -> b

-- map applis a function to each member of a list and returns a list, while a fold replaces the cons constructors with the function and reduces the list

-- 10.3 Recursive patterns - base case is identity of the function

-- 10.4 Fold Right

-- right fold because fold is right associative; it associates to the right

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f acc []     = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

-- acc is the accumulator, usually the identity of a function, i.e. 0 for (+) and 1 for (*)

foldrr :: (a -> b -> b) -> b -> [a] -> b
foldrr f acc xs =
  case xs of
    []     -> acc
    (x:xs) -> f x (foldrr f acc xs)


anon = (\_ _ -> 9001)

-- const :: a -> b -> a
-- const x _ = x

-- 10.5 Fold Left

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f acc [] = acc
-- foldl f acc (x:xs) = foldl f (f acc x) xs

--foldr (+) 0 [1..5] vs foldl (+) 0 [1..5]
--(1 +(2 +(3 +(4 +(5+0))))) vs (((((0+1)+ 2)+ 3)+ 4)+ 5)
