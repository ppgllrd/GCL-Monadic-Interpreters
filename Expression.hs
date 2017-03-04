--------------------------------------------------------------------------------
-- Pepe Gallardo, January 2004
--
-- A monadic interpreter for Dijkstra's Guarded Command Language:
-- https://en.wikipedia.org/wiki/Guarded_Command_Language
--
-- Expressions
--------------------------------------------------------------------------------

module Expression
  ( Variable
  , Value(I,B)
  , Expr(..)
  , Environment
  , eval
  )where

import qualified Environment as Env

type Variable = String

data Value = I Int | B Bool

instance Show Value where
  show (I n) = show n
  show (B b) = show b

infixl 7 :*, :/
infixl 6 :+, :-
infix  4 :>, :==
infixl 3 :/\

-- Expressions supported by our lenguage
data Expr = Const Value          -- constants
          | Var Variable         -- variables
          | Expr :+  Expr        -- addition
          | Expr :-  Expr        -- substraction
          | Expr :*  Expr        -- product
          | Expr :/  Expr        -- división
          | Expr :>  Expr        -- greater than
          | Expr :== Expr        -- equal to
          | Expr :/\ Expr        -- logical conjunction

type Environment = Env.Environment Variable Value

-- Evaluating an expression in an environment
eval :: (Monad m) => Environment -> Expr -> m Value
eval rho (Const c)   = return c
eval rho (Var v)     = Env.get rho v
eval rho (e1 :+ e2)  = operateIxIxI rho e1 (+) e2
eval rho (e1 :- e2)  = operateIxIxI rho e1 (-) e2
eval rho (e1 :* e2)  = operateIxIxI rho e1 (*) e2
eval rho (e1 :/ e2)  = do
  I v1 <- eval rho e1
  I v2 <- eval rho e2
  if v2==0 then fail "Division by zero" else return (I (div v1 v2))
eval rho (e1 :> e2)  = operateIxIxB rho e1 (>)  e2
eval rho (e1 :== e2) = operateIxIxB rho e1 (==) e2
eval rho (e1 :/\ e2) = operateBxBxB rho e1 (&&) e2

type IxIxI = Int  -> Int  -> Int
type IxIxB = Int  -> Int  -> Bool
type BxBxB = Bool -> Bool -> Bool

operateIxI :: (Monad m) => (a -> Value) -> Environment -> Expr -> (Int -> Int -> a) -> Expr -> m Value
operateIxI constr rho e1 op e2 = do
  I v1 <- eval rho e1
  I v2 <- eval rho e2
  return (constr (v1 `op` v2))

operateIxIxI :: (Monad m) => Environment -> Expr -> IxIxI -> Expr -> m Value
operateIxIxI = operateIxI I

operateIxIxB :: (Monad m) => Environment -> Expr -> IxIxB -> Expr -> m Value
operateIxIxB = operateIxI B

operateBxBxB :: (Monad m) => Environment -> Expr -> BxBxB -> Expr -> m Value
operateBxBxB rho e1 op e2 = do
  B v1 <- eval rho e1
  B v2 <- eval rho e2
  return (B (v1 `op` v2))
