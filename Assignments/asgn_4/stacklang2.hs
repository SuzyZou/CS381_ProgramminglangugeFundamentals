-- Student Name : Ya Zou

--   ************Part 2: Stack Language 2***
--   ************Operations****************

-- LDI Int – loads its integer parameter onto the stack (replaces LD)
-- LDB Bool – loads its boolean parameter onto the stack.
-- ADD – same as with Stack Language 1 only adds integers
-- MULT – same as with Stack Language 1 only multiplies integers
-- DUP – places a copy of the stack’s topmost element on the stack. If the stack is empty then DUP produces an error. Works with both integer and Boolean values.
-- LEQ – removes the top integer from the stack, removes the second integer from the stack. If top ≤ second the True is pushed on the stack else False is pushed onto the stack.
-- IFELSE Prog Prog - if the value on top of the stack is true, then run the first program, else run the second program.

--  DUP – places a copy of the stack’s topmost element on the stack. If the stack is empty then   DUP produces an error.


type Prog = [Cmd]

type Stack = [Either Bool Int]
s :: Stack
s = [Right 1, Right 2,Right 3, Left True, Right 6]

data Cmd = LDI Int
         | LDB Bool 
         | LEQ
         | ADD
         | MULT
         | DUP
         | IFELSE Prog Prog 
         deriving Show



sem :: Cmd -> Stack -> Maybe Stack

sem (LDI x) xs = Just (Right x:xs)
sem (LDB b) s = Just (Left b: s)

sem LEQ [] = Nothing
sem LEQ ((Right x):[]) = Nothing
sem LEQ ((Right x):(Right y):s) = Just ((Left(x<=y)):s)
sem LEQ _ = Nothing

sem ADD [] = Nothing 
sem ADD (Right x:[]) = Nothing
sem ADD (Right x:Right y:xs) = Just (([Right(x+y)])++xs)
sem ADD _ =Nothing

sem MULT [] = Nothing 
sem MULT (Right x:[]) = Nothing
sem MULT (Right x: Right y:xs) = Just (([Right(x*y)])++xs)
sem MULT _ =Nothing

sem DUP [] = Nothing
sem DUP  (x:s)  = Just (x:(x:s))

sem (IFELSE p1 p2) (x:s) = case x of
                            Left True  -> run p1 s
                            Left False -> run p2 s
                            _          -> Nothing

run :: Prog -> Stack -> Maybe Stack
run []        s = Just s
run (x:xs) s = case sem x s of 
             Nothing -> Nothing
             Just r -> run xs r


-- ***********************Teset info*******************
-- stack1 :: Stack
-- stack1 = [Right 1, Right 3, Right 5, Right 7, Right 9]
-- stack2 :: Stack
-- stack2 = [Left True,Right 3]
-- test1 = [LDI 3,DUP,ADD,DUP,MULT]
-- test2 = [LDB True, DUP, IFELSE[LDI 1][LDI 0]]
-- test3 = [LEQ]
-- test4 = [ADD,ADD,MULT,DUP]
-- test5 = [LEQ,IFELSE [] [], LDI 9]
-- test6 = [LDI 5, LDI 2, LEQ, IFELSE [LDI 10, DUP] [], ADD]
-- test7 = [LDI 5, LDI 7, LEQ, IFELSE [LDI 10, DUP] [LDI 20, DUP],ADD]
-- test8 = [LDI 1, LDI 2, LDI 3, LDI 4, LDI 5, ADD, ADD, ADD, ADD]
