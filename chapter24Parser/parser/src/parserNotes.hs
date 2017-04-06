{-# LANGUAGE InstanceSigs #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State
import           System.Random

import           Text.Trifecta

-- 24.1 Parser combinators
-- core idea is to accept serialized input in sequence of characters and turn it into a structured data type

-- 24.2

-- 24.3 understanding the parsing process

-- a parser combinator is a higher order function that takes parsers as input and returns a new parser as output

stop :: Parser a
stop = unexpected "stop"

-- unexpected is a means of throwing errors

one = char '1'

one' = one >> stop

-- (>>) :: Monad m => m a -> m b -> m b

-- like state with failure

-- type Parser a = String -> Maybe (a, String)

-- char :: Char -> Parser Char
-- char c =
--   Parser $ \ s ->
--     case s of
--       (x:xs) -> if c == x
--                then [(c, xs)]
--                else []
--       _ -> []

-- get :: Monad m => StateT s m s
-- put :: Monad m => s -> StateT s m ()
-- runStateT :: StateT s m a -> s -> m (a, s)


oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
