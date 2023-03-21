--
-- Type Checker for the Expression Language
--
module TypeCheck where

import Expr2


-- Expr data type and examples are imported from module Expr2
--
-- data Expr = N Int
-- 	  | Plus Expr Expr
-- 	  | Equal Expr Expr
-- 	  | Not Expr
--    deriving Show
--
-- x = N 2
-- y = Plus (N 3) (Plus (N 4) (N 5))
-- z = Not (Equal (Plus (N 1) (N 1)) (N 2))
-- typeError = Not (N 2)
-- e1 = N 2 `Equal` N 2
-- e2 = N 1 `Equal` e1
-- e3 = Not (N 2 `Plus` N 2)

-- A data type to represent types
--
data Type = Int | Bool | TypeError
            deriving (Eq,Show)

-- In addition to deriving Show for printing types, we add
-- a "deriving Eq" clause to be able to compare Type values
-- for equality.


-- The type checker uses guards to constrain the selection
-- of definitions by conditions about the type of subexpressions.
--
tc :: Expr -> Type
tc (N i)                                    = Int

-- if the expression is an addition expression, the function recursively checks the types of the two sub-expressions e and e' using tc, and then checks if both are Int. If they are both Int, the function returns the type Int.
tc (Plus e e')  | tc e==Int  && tc e'==Int  = Int
tc (Equal e e') | tc e==Int  && tc e'==Int  = Bool
                | tc e==Bool && tc e'==Bool = Bool
tc (Not e)      | tc e==Bool                = Bool
tc _                                        = TypeError


-- We can use the type checker to perform only safe evaluations
--
-- typeCorrect: This function takes an Expr as its argument and checks if the type of the expression is correct by calling the tc function. If the tc function returns a TypeError, the typeCorrect function returns False. Otherwise, it returns True.
typeCorrect :: Expr -> Bool
typeCorrect e = tc e /= TypeError


-- evalStatTC: This function takes an Expr as its argument and first checks if the type of the expression is correct by calling the typeCorrect function. If typeCorrect returns True, the function evaluates the expression using the eval function and returns a Just value containing the result. If typeCorrect returns False, the function returns Nothing.

evalStatTC :: Expr -> Maybe Val
evalStatTC e | typeCorrect e = Just (eval e)
             | otherwise     = Nothing


-- compare "evalStatTC e2" and "eval e2"
