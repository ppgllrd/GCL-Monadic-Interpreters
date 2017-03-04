--------------------------------------------------------------------------------
-- Pepe Gallardo, January 2004
--
-- A monadic interpreter for Dijkstra's Guarded Command Language:
-- https://en.wikipedia.org/wiki/Guarded_Command_Language
--
-- Some programming examples
--------------------------------------------------------------------------------

module Examples
  ( simple
  , divCero
  , nonDeterm
  , nonDetermError
  , gcDiv
  , urn
  , examples
  ) where

import Expression
import Command

integer = Const . I
bool = Const . B

zero = integer 0
one = integer 1
two = integer 2
three = integer 3

varX = Var "X"
varY = Var "Y"
varZ = Var "Z"
varB = Var "B"
varW = Var "W"


-- Simple example
simple =
  "X" := one                 :$
  "Y" := varX :+ two         :$
  "Z" := two :* varY :- varY


-- Division by zero error
divCero =
   "X" := zero         :$
   "Y" := one :/ varX  :$
   "Z" := three


-- Non deterministic selection
--
--          (3,3)
--        /      \
--    (2,3)      (3,2)
--   /     \    /    \
-- (1,3) (2,2)(2,2) (3,1)
nonDeterm =
  "X" := three    :$
  "Y" := three    :$
  nonDetermIf     :$
  nonDetermIf
   where
     nonDetermIf =
       If [ varX :> zero  :->  "X" := varX :- one
          , varY :> zero  :->  "Y" := varY :- one
          ]


-- Non deterministic repetition (with errors)
--
--                         (2,1)
--                           |
--          -----------------+----
--         /                 |    \
--        /                  |     \
--     (1,1)               (2,0)  Error       <--- First iteration
--     /   \               /   \     \
-- (0,1)   (1,0)       (1,0)  Error   \       <--- Second iteration
--   |     /   \       /  \      \     \
-- (0,0) (0,0) Error (0,0) Error Error Error  <--- Third iteration
nonDetermError =
  "X" := two    :$
  "Y" := one    :$
  Do [ varX :> zero  :->  "X" := varX :- one
     , varY :> zero  :->  "Y" := varY :- one
     , varX :> varY  :->  "X" := varY :/ zero
     ]

-- Deterministic repetition
gcDiv x y =
  "X" := integer x  :$
  "Y" := integer y  :$
  Do [ varX :> varY   :->  "X" := varX :- varY
     , varY :> varX   :->  "Y" := varY :- varX
     ]

-- Non deterministic repetition. Urns' problem
urn w b =
  "W" := integer w  :$
  "B" := integer b  :$
  Do [ varB :> one   :->  "B" := varB :- one
     , varW :> one   :->  "B" := varB :+ one :$
                          "W" := varW :- two
     , varW :> zero
        :/\
       varB :> zero  :->  "B" := varB :- one
     ]

examples =
  [ simple
  , divCero
  , nonDeterm
  , nonDetermError
  , gcDiv 100 25
  , urn 2 2
  ]
