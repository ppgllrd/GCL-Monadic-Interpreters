--------------------------------------------------------------------------------
-- Pepe Gallardo, January 2004
--
-- A monadic interpreter for Dijkstra's Guarded Command Language:
-- https://en.wikipedia.org/wiki/Guarded_Command_Language
--
-- Fourth version. Uses state transformer + non-deterministic + error monad
--
--------------------------------------------------------------------------------

import qualified Environment as Env
import Expression
import Command
import Examples
import Data.List(intercalate)
import Control.Monad


--------------------------------------------------------------------------------
-- The (state transformer + non-deterministic + error) monad
--------------------------------------------------------------------------------

data Result a = Ok a | Error String
newtype Mon a = M (Environment -> [Result (a, Environment)])

instance Monad Mon where
  -- return :: a -> Mon a
  return x  = M (\s -> [ Ok (x, s) ])

  -- fail :: String -> Mon a
  fail str  = M (\_ -> [ Error str ])

  -- (>>=) :: Mon a -> (a -> Mon b) -> Mon b
  M st >>= f  = M (\s -> concat [ apply f m | m <- st s ])
    where
      apply f (Ok (x, s)) = let M st = f x in st s
      apply f (Error str) = [ Error str ]

instance MonadPlus Mon where
  -- mzero :: Mon a
  mzero  = M (\_ -> [])

  -- mplus :: Mon a -> Mon a -> Mon a
  M st0 `mplus` M st1  = M (\s -> st0 s ++ st1 s)

readMon :: Mon Environment
readMon = M (\s -> [ Ok (s, s) ])

writeMon :: Environment -> Mon ()
writeMon s = M (\_ -> [ Ok ((), s) ])


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
run prog = sequence_ [ putStrLn (showResult r)
                     | r <- results
                     ]
 where
   M st     = sem prog
   results  = st Env.empty
   showResult (Ok (_, rho)) =
       intercalate ", "
     . Env.fold (\var val -> ((var++"->"++show val):)) []
     $ rho
   showResult (Error str) =
     "Error: "++str


--------------------------------------------------------------------------------
-- Run all examples
--------------------------------------------------------------------------------

main = sequence_ [ do run e; putStr "\n" | e <- examples ]
