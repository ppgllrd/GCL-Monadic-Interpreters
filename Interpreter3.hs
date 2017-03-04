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
import Control.Monad


--------------------------------------------------------------------------------
-- The (state transformer) monad
--------------------------------------------------------------------------------

newtype Mon a = M (Environment -> [(a, Environment)])

instance Monad Mon where
  -- return :: a -> Mon a
  return x  = M (\s -> [(x,s)])

  -- fail :: String -> Mon a
  fail str  = M (\_ -> error str)

  -- (>>=) :: Mon a -> (a -> Mon b) -> Mon b
  M st0 >>= f  = M (\s0 -> concat [ st1 s1
                                  | (x, s1) <- st0 s0
                                  , let M st1 = f x
                                  ])

instance MonadPlus Mon where
  -- mzero :: Mon a
  mzero  = M (\_ -> [])

  -- mplus :: Mon a -> Mon a -> Mon a
  M st0 `mplus` M st1  = M (\s -> st0 s ++ st1 s)

readMon :: Mon Environment
readMon  = M (\s -> [(s, s)])

writeMon :: Environment -> Mon ()
writeMon s  = M (\_ -> [((), s)])


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
    ss -> foldr1 mplus . map sem  $  ss
sem (Do gs) = do
  rho <- readMon
  ss <- select rho gs
  case ss of
		[]  -> return ()
		ss  -> foldr1 mplus . map (sem . (:$ Do gs))  $  ss

-- Running a program. Prints out final environment
run :: Command -> IO ()
run prog = sequence_ [ printEnv rho `catch` \error -> print (error :: ErrorCall)
                     | rho <- rhos
                     ]
 where
   M st     = sem prog
   rhos     = [ rho | (_, rho) <- st Env.empty ]
   printEnv = putStrLn
            . intercalate ", "
            . Env.fold (\var val -> ((var++"->"++show val):)) []


--------------------------------------------------------------------------------
-- Run all examples
--------------------------------------------------------------------------------

main = sequence_ [ do run e; putStr "\n" | e <- examples ]
