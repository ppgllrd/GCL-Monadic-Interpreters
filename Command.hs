--------------------------------------------------------------------------------
-- Pepe Gallardo, January 2004
--
-- A monadic interpreter for Dijkstra's Guarded Command Language:
-- https://en.wikipedia.org/wiki/Guarded_Command_Language
--
--
-- Abstract Syntax Tree of language
--------------------------------------------------------------------------------

module Command
  ( Command(..)
  , GuardedCommand(..)
  , select
  ) where

import Expression

infix  3 :=
infixr 2 :$
infix  1 :->

data Command =  Skip                -- No operation
              | Abort                -- Abort program
              | Variable := Expr     -- Variable assigment
              | Command :$ Command  -- Sequence
              | If [GuardedCommand]  -- Guarded sentence
              | Do [GuardedCommand]  -- Guarded repetition

data GuardedCommand = Expr :-> Command

select :: (Monad m) => Environment -> [GuardedCommand] -> m [Command]
select rho []             = return []
select rho ((b :-> s):gs) = do
  B b <- eval rho b
  ss  <- select rho gs
  return (if b then (s:ss) else ss)
