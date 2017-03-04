--------------------------------------------------------------------------------
-- Pepe Gallardo, January 2004
--
-- A monadic interpreter for Dijkstra's Guarded Command Language:
-- https://en.wikipedia.org/wiki/Guarded_Command_Language
--
-- Environments for associating mutable variables to their values
--------------------------------------------------------------------------------

module Environment(Environment, empty, get, set, fold) where

import Data.List(intercalate)

newtype Environment a b = Env [(a, b)]

instance (Show a, Show b) => Show (Environment a b) where
  show (Env as) =
    intercalate ", " [ show var++"->"++show val | (var,val) <- as ]

empty :: Environment a b
empty = Env []

get :: (Monad m, Eq a, Show a) => Environment a b -> a -> m b
get (Env as) v = search as v
  where
    search []             _ = fail (show v++" is undefined")
    search ((var,val):as) v
      | var == v            = return val
      | otherwise           = search as v

set :: (Monad m, Eq a) => Environment a b -> (a, b) -> m (Environment a b)
set (Env as) pair = return (Env (aux as pair))
  where
   aux [] pair     = [pair]
   aux (p@(var,val):as) p'@(var',val')
     | var == var' = p':as
     | otherwise   = p : aux as p'

fold :: (a -> b -> c -> c) -> c -> Environment a b -> c
fold f z (Env as) = foldr (uncurry f) z as
