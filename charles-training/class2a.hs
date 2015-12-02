-- interpreter - reduce expression to simplest form
-- compiler - transform without losing information

data Exp = 
      Add Exp Exp
    | Mul Exp Exp
    | Const Int

eval :: Exp -> Int
eval (Const x) = x 
eval (Add x y) = (eval x) + (eval y) 
eval (Mul x y) = (eval x) * (eval y)

data Instr = PUSH Int | ADD | MUL deriving Show

-- 2 * (3 + 4)
-- PUSH 2
-- PUSH 3
-- PUSH 4
-- ADD 
-- MUL
compile :: Exp -> [Instr]
compile (Const x) = [PUSH x]
compile (Add x y) = (compile x) ++ (compile y) ++ [ADD]
compile (Mul x y) = (compile x) ++ (compile y) ++ [MUL]

compileC :: Exp -> String
compileC (Const x) = show x
compileC (Add x y) = "(" ++ (compileC x) ++ "+" ++ (compileC y) ++ ")"
compileC (Mul x y) = "(" ++ (compileC x) ++ "*" ++ (compileC y) ++ ")"

testExpr = Mul (Const 2) (Add (Const 3) (Const 4))

main = do
    print $ let answer = eval testExpr in case answer == 14 of
        True  -> "You got it right!"
        False -> "Uh oh. Expected 14 as output but got " ++ (show answer)
    print $ compile testExpr
    print $ compileC testExpr
