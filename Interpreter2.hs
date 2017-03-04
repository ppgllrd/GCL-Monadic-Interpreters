--------------------------------------------------------------------------------
-- Pepe Gallardo, January 2004
--
-- A monadic interpreter for Dijkstra's Guarded Command Language:
-- https://en.wikipedia.org/wiki/Guarded_Command_Language
--
-- Second version. Uses state transformer monad to propagate environments
--
-- Drawbacks: * Language is deterministic
--			      * Errors abort interpreter execution
--------------------------------------------------------------------------------

import qualified Environment as Env
import Expression
import Command
import Examples
import Data.List(intercalate)
import Control.Exception(catch, ErrorCall)

--------------------------------------------------------------------------------
-- The (state transformer) monad
--------------------------------------------------------------------------------

newtype Mon a = M (Environment -> (a, Environment))

instance Monad Mon where
  -- return :: a -> Mon a
  return x  = M (\s -> (x,s))

  -- fail :: String -> Mon a
  fail str  = M (\_ -> error str)

  -- (>>=) :: Mon a -> (a -> Mon b) -> Mon b
  M st0 >>= f  = M (\s0 ->
    let (x, s1) = st0 s0
        M st1  = f x
    in
      st1 s1)

readMon :: Mon Environment
readMon  = M (\s -> (s, s))

writeMon :: Environment -> Mon ()
writeMon s  = M (\_ -> ((), s))


--------------------------------------------------------------------------------
-- Sematics of commands
--------------------------------------------------------------------------------

sem :: Command -> Mon ()
sem Skip =
  return ()
sem Abort =
  fail "Program aborted"
sem (var := e) = do
  rho <- readMon
  val <- eval rho e
  rho' <- Env.set rho (var, val)
  writeMon rho'
sem (s1 :$ s2) = do
  sem s1
  sem s2
sem (If gs) = do
  rho <- readMon
  ss <- select rho gs
  case ss of
		[]  -> fail "Program aborted: all guards are false"
		s:_ -> sem s
sem (Do gs) = do
  rho <- readMon
  ss <- select rho gs
  case ss of
		[]  -> return ()
		s:_ -> sem (s :$ Do gs)

-- Running a program. Prints out final environment
run :: Command -> IO ()
run prog = printEnv rho `catch` \error -> print (error :: ErrorCall)
 where
   M st           = sem prog
   (_, rho)       = st Env.empty
   printEnv = putStrLn
            . intercalate ", "
            . Env.fold (\var val -> ((var++"->"++show val):)) []


--------------------------------------------------------------------------------
-- Run all examples
--------------------------------------------------------------------------------

main = sequence_ [ do run e; putStr "\n" | e <- examples ]
