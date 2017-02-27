import           Data.List

--14.1 Testing
-- what is testing, using testing libraries, morse code

--14.2 - Tour of testing
-- Unit testing vs property testing
-- unit testing tests the smallest atomic units of software independently
-- spec testing is a newer version of unit testing
-- property testing tests the formal properties of programs without requiring formal proofs by allowing you to express a truth-valued, universally quantified function

-- 14.2 - conventional testing

-- f :: Int -> [Int] -> [Int]
-- f _ [] = []
-- f n (x:xs) = replicate n x ++ (f n xs)

-- f :: [Int] -> [Int]
-- f []  = []
-- f lst = filter (\x -> even (1 + pullMaybe (elemIndex x lst))) lst

pullMaybe :: Maybe a -> a
pullMaybe (Just a) = a
