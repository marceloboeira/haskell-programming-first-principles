
-- 13.1 Modules

-- 13.2 Making packages with Stack

-- 13.3 Working with a basic project

-- 13.4 Making our project a library

-- 13.5 Module exports - when you dont specify any exports in a module, every top level binding is exported

-- Exposing Modules (src/DogsRule.hs)
-- we have to expose dogsRule module in library file

-- 13.6 More on importing modules

import           Data.Bool
-- this is an unqualified import of everything in Data.Bool

-- Qualified imports - a way to deal with importing different modules with the same name

-- Intermission

-- 1
-- importing functions forever, when from Control.Monad

-- 2
--Data.Bits and Database.Blacktip.Types are imported unqualified in entirety

-- 3
-- Blacktip types

-- 4
-- a) Control.concurrent.MVar, Filesystem.Path.CurrentOS, Control.Concurrent

-- b) Filesystem

-- c) forever comes from Control.Monad import

-- 13.7 Making our program interactive

-- 13.8 do syntax and IO


-- main = do
--   x1 <- getLine
--   x2 <- getLine
--   return (x1 ++ x2)

-- return - returns a value inside the monadic structure
-- :t return :: Monad m => a -> m a
-- return returns a value in IO

twoo :: IO Bool
twoo = do c  <- getChar
          c' <- getChar
          return (c == c')

main :: IO ()
main = do c <- getChar
          c' <- getChar
          if c == c'
            then putStrLn "True"
            else return ()
-- it is bad form to use do blocks for single line expressions (will learn to use >>= instead of do)

-- 13.9 Hangman game
-- see
