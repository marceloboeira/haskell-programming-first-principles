import           Data.Char
import           Data.List

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

-- As-Patterns - a way to be able to pattern match on part of something

-- 1

isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' a@(x:xs) b@(y:ys)
  | elem x b = isSubsequenceOf' xs b
  | otherwise = False

-- 2
squish :: [[a]] -> [a]
squish list = foldr (++) [] list

capitalizeWords :: String -> [(String, String)]
capitalizeWords [] = []
capitalizeWords sentence = (x, upX) : capitalizeWords a
  where (x:xs) = (words sentence)
        upX = (toUpper (head x)) : (tail x)
        a = unwords xs

-- Language exercises

-- 1
capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

-- 2
--capitalizeParagraph :: String -> String
--capitalizeParagraph "" = ""
--capitalizeParagraph words'
--   | last x == '.' = unwords (x : (capitalizeWord (head xs)) : [capitalizeParagraph (unwords xs)])
--   | otherwise = unwords (x : [capitalizeParagraph (unwords xs)])
--   where (x:xs) = words words'

--capNext :: [String] -> [String]
--capNext (x:x':xs) = x : capitalizeWord x' : xs

exists :: [a] -> Bool
exists [] = False
exists _  = True

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph words'
   | last x == '.' && exists xs = unwords (x : (capitalizeWord (head xs)) : [capitalizeParagraph (unwords (tail xs))])
   | otherwise = unwords (x : [capitalizeParagraph (unwords xs)])
  where (x:xs) = words words'

completeParagraph :: String -> String
completeParagraph para = capitalizeParagraph $ (toUpper(head para)) : (tail para)

-- there is obviously a more elegant way to do this... guessing at first letter to capitalize

-- Phone Exercise

data Keypad = Zero' | One' | Two' | Three' | Four' | Five' | Six' | Seven' | Eight' | Nine' | Star' deriving (Eq, Show)

type Repeats = Int

data Zero' = Space' deriving (Eq, Ord, Show)
data One' = One deriving (Eq, Ord, Show)
data Two' = A | B | C | Two deriving (Eq, Ord, Show)
data Three' = D | E | F | Three deriving (Eq, Ord, Show)
data Four' = G | H | I | Four deriving (Eq, Ord, Show)
data Five' = J | K | L | Five deriving (Eq, Ord, Show)
data Six' = M | N | O | Six deriving (Eq, Ord, Show)
data Seven' = P | Q | R | S | Seven deriving (Eq, Ord, Show)
data Eight' = T | U | V | Eight deriving (Eq, Ord, Show)
data Nine' = W | X | Y | Z | Nine deriving (Eq, Ord, Show)
--data Star' =

data PhoneCode = PhoneCode Keypad Repeats deriving (Eq, Show)

zero = [' ']
one = ['1']
two = ['A','B','C','2']
three = ['D','E','F','3']
four = ['G','H','I','4']
five = ['J','K','L','5']
six = ['M','N','O','6']
seven = ['P','Q','R','S','7']
eight = ['T','U','V','8']
nine = ['W','X','Y','Z','9']

allLetters = [zero,one,two,three,four,five,six,seven,eight,nine]

convert1 :: PhoneCode -> Char
convert1 (PhoneCode Zero' n)  = zero !! (mod (n-1) 0)
convert1 (PhoneCode One' n)   = one !! (mod (n-1) 1)
convert1 (PhoneCode Two' n)   = two !! (mod (n-1) 4)
convert1 (PhoneCode Three' n) = three !! (mod (n-1) 4)
convert1 (PhoneCode Four' n)  = four !! (mod (n-1) 4)
convert1 (PhoneCode Five' n)  = five !! (mod (n-1) 4)
convert1 (PhoneCode Six' n)   = six !! (mod (n-1) 4)
convert1 (PhoneCode Seven' n) = seven !! (mod (n-1) 5)
convert1 (PhoneCode Eight' n) = eight !! (mod (n-1) 4)
convert1 (PhoneCode Nine' n)  = nine !! (mod (n-1) 5)

convo :: [String]
convo = ["Wanna play 20 questions",
         "Ya",
         "U 1st haha",
         "Lol ok Have u ever tasted alcohol lol",
         "Lol ya",
         "Wow ur cool haha Ur turn",
         "Ok Do u think i am pretty Lol",
         "Lol ya",
         "Haha thanks just making sure rofl ur turn"]

findList :: Char -> [[Char]] -> [Char]
findList _ [] = []
findList a (x:xs)
  | elem (toUpper a) x = x
  | otherwise = findList (toUpper a) xs

findKey :: [Char] -> Keypad
findKey list
  | list == zero = Zero'
  | list == one = One'
  | list == two = Two'
  | list == three = Three'
  | list == four = Four'
  | list == five = Five'
  | list == six = Six'
  | list == seven = Seven'
  | list == eight = Eight'
  | list == nine = Nine'
  | otherwise = error "list not in scope"

findKeypad :: Char -> [Char] -> PhoneCode
findKeypad a list
   | elemIndex (toUpper a) list == (Just n) = (PhoneCode key (n+1))
   | otherwise = error "not on list"
  where key = findKey list
        (Just n) = elemIndex (toUpper a) list


reverseTaps :: [Char] -> [PhoneCode]
reverseTaps []     = []
reverseTaps (x:xs) = findKeypad (toUpper x) (findList x allLetters) : reverseTaps xs

reverseTapsList :: [String] -> [[PhoneCode]]
reverseTapsList []     = []
reverseTapsList (x:xs) = reverseTaps x : reverseTapsList xs

-- 3

addTaps :: PhoneCode -> Repeats -> Repeats
addTaps (PhoneCode _ n) acc = n + acc

totalTaps :: [PhoneCode] -> Repeats
totalTaps list = foldr addTaps 0 list

messageTaps :: String -> Repeats
messageTaps message = totalTaps (reverseTaps message)

-- 4


-- Hutton's Razor - expresses integer literals and addition of values in that expression language

-- 1

data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit int) = int
eval (Add a b) = (eval a) + (eval b)

-- 2

printExpr :: Expr -> String
printExpr (Lit int) = show int
printExpr (Add a b) = (printExpr a) ++ " + " ++ (printExpr b)
