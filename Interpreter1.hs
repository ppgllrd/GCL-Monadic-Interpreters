--------------------------------------------------------------------------------
-- Pepe Gallardo, January 2004
--
-- A monadic interpreter for Dijkstra's Guarded Command Language:
-- https://en.wikipedia.org/wiki/Guarded_Command_Language
--
-- Initial version (without non-determinism neither environments)
--
-- Drawbacks: * Language is deterministic
--            * We must propagate enviroment through different
--              functions making use of it
--            * Errors abort interpreter execution
--------------------------------------------------------------------------------

import qualified Environment as Env
import Expression
import Command
import Examples
import Data.List(intercalate)
import Control.Exception(catch, ErrorCall)

--------------------------------------------------------------------------------
-- The (identity) monad
--------------------------------------------------------------------------------

newtype Mon a = M a deriving Show

instance Monad Mon where
  -- return :: a -> Mon a
  return x  = M x

  -- fail :: String -> Mon a
  fail str  = error str

  -- (>>=) :: Mon a -> (a -> Mon b) -> Mon b
  M x >>= f  = f x


--------------------------------------------------------------------------------
-- Sematics of commands
--------------------------------------------------------------------------------

-- Returns new Environment after executing sentence
sem :: Environment -> Command -> Mon Environment
sem rho Skip =
  return rho
sem rho Abort =
  fail "Program aborted"
sem rho (var := e) = do
  v <- eval rho e
  rho' <- Env.set rho (var,v)
  return rho'
sem rho (s1 :$ s2) = do
  rho'  <- sem rho  s1
  rho'' <- sem rho' s2
  return rho''
sem rho (If gs) = do
  ss <- select rho gs
  case ss of
    []  -> fail "Program aborted: all guards are false"
    s:_ -> sem rho s
sem rho (Do gs) = do
  ss <- select rho gs
  case ss of
    []  -> return rho
    s:_ -> sem rho (s :$ Do gs)

-- Running a program. Prints out final environment
run :: Command -> IO ()
run prog = printEnv rho `catch` \error -> print (error :: ErrorCall)
  where
   M rho = sem Env.empty prog
   printEnv = putStrLn
            . intercalate ", "
            . Env.fold (\var val -> ((var++"->"++show val):)) []


--------------------------------------------------------------------------------
-- Run all examples
--------------------------------------------------------------------------------

main = sequence_ [ do run e; putStr "\n" | e <- examples ]


