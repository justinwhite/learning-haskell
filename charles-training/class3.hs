-- from https://www.hackerrank.com/challenges/while-language-fp

import qualified Data.Map as Map

data Opa = Add
         | Sub
         | Mul
         | Div

data Opr = Lt
         | Gt

data Opb = And
         | Or

data BExp = BoolLit Bool
          | BBExp Opb BExp BExp
          | RBExp Opr AExp AExp

data AExp = Var String
          | IntLit Int
          | AExp Opa AExp AExp

data Stmt = Assign String AExp
          | Sequence Stmt Stmt
          | While BExp Stmt
          | If BExp Stmt Stmt
          
type Env = Map.Map String Int

evalB :: (Env, BExp) -> Bool
evalB (env, e) = case e of
  BoolLit b -> b

  RBExp op a1 a2 -> let
    a1' = evalA (env, a1)
    a2' = evalA (env, a2)
    in case op of
      Lt -> a1' < a2'
      Gt -> a1' > a2'
    
  BBExp op b1 b2 -> let
    b1' = evalB (env, b1)
    b2' = evalB (env, b2)
    in case op of  
      And -> (&&) b1' b2'
      Or  -> (||) b1' b2'
    
-- DONE
evalA :: (Env, AExp) -> Int
evalA (env, e) = case e of
  Var s -> env Map.! s -- precondition: s is in the environment

  IntLit i -> i

  -- AExp op e1 e2 -> let
  -- f = case op of
  --   Add -> (+)
  --   Sub -> (-)
  --   Mul -> (*)
  --   Div -> div
  -- in f (evalA (env, e1)) (evalA (env, e2))
  AExp op e1 e2 -> let
    x = evalA (env, e1)
    y = evalA (env, e2)
    in case op of
      Add -> (+) x y
      Sub -> (-) x y 
      Mul -> (*) x y
      Div -> div x y

interpret :: (Env, Stmt) -> Env
interpret (env, s) = case s of
  Assign var ex -> Map.insert var (evalA (env, ex)) env
  Sequence s1 s2 -> interpret (interpret (env, s1), s2)
  If b s1 s2 -> case (evalB (env, b)) of 
    True -> interpret (env, s1) 
    False -> interpret (env, s2)
  While b block -> case evalB (env, b) of
    True -> -- interpet (env, Sequence block s)
      let env' = interpret (env, block)
      in  interpret (env', s)
    False -> env

showEnv :: Env -> String
showEnv env = let 
    f = \(k,v) -> k ++ " " ++ show v
    in unlines $ map f $ Map.toList env

type Program = Stmt
runProgram :: Program -> Env
runProgram s = interpret (Map.empty, s)

main = putStrLn $ showEnv $ runProgram program

{-
fact := 1 ;
val := 10000 ;
cur := val ;
mod := 1000000007 ;

while ( cur > 1 )
  do
   {
      fact := fact * cur ;
      fact := fact - (fact / mod)* mod ;
      cur := cur - 1
   } ;

cur := 0
-}
program = Sequence (Assign "fact" (IntLit 1))
  $ Sequence (Assign "val" (IntLit 10000))
  $ Sequence (Assign "cur" (Var "val"))
  $ Sequence (Assign "mod" (IntLit 1000000007))
  $ Sequence (While (RBExp Gt (Var "cur") (IntLit 1))
    $ Sequence (Assign "fact" (AExp Mul (Var "fact") (Var "cur")))
    $ Sequence (Assign "fact" (AExp Sub (Var "fact") (AExp Mul (AExp Div (Var "fact") (Var "mod")) (Var "mod"))))
    $ Assign "cur" (AExp Sub (Var "cur") (IntLit 1))
  ) $ Assign "cur" (IntLit 0)
