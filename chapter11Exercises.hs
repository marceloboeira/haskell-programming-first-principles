import           Data.Char

-- 11.16 Chapter Exercises

-- 1

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday

-- Weekday is a type with five data constructors

-- 2
f :: Weekday -> String
f Friday = "Miller Time"

-- 3
-- types defined with the data keyword must begin with a capital letter

-- 4

g xs = xs !! (length xs - 1)
-- g delivers the last element of a list

-- Ciphers (Vigenere) -- uses a series of caesar ciphers for polyalphabetic subsitution
-- substitution for each letter in the plalintext is determined by fixed keyword

type Keyword = [Char]
type Message = [Char]
type Code = [Char]

shift :: Char -> Int -> Char
shift a n = chr (65 + (mod (((ord (toUpper a)) - 65) + n) 26))

infiniteKey :: [Char] -> [Char]
infiniteKey key = key ++ infiniteKey key

vigCipher :: Message -> Keyword -> Code
vigCipher [] _          = []
vigCipher (x:xs) key
  | ord (toUpper x) > 90 = x : vigCipher xs key
  | ord (toUpper x) < 65 = x : vigCipher xs key
  | otherwise = shift x ((ord (toUpper y))-65) : vigCipher xs ys
  where (y:ys) = infiniteKey key

deVigCipher :: Code -> Keyword -> Message
deVigCipher [] _ = []
deVigCipher (x:xs) key
  | ord (toUpper x) > 90 = x : deVigCipher xs key
  | ord (toUpper x) < 65 = x : deVigCipher xs key
  | otherwise = shift x (-(ord (toUpper y))-65) : deVigCipher xs ys
  where (y:ys) = infiniteKey key

-- As-Patterns
