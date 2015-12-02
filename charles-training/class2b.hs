--{-# LANGUAGE DeriveFunctor #-}
import Data.Functor

--Given a data structure

instance Functor ExpF where
    fmap f = \e -> case e of
        (Add x y) -> Add (f x) (f y)
        (Mul x y) -> Mul (f x) (f y)
        (Const x) -> Const x

data ExpF a =
    Add a a
  | Mul a a
  | Const Int

--And Fix magic

newtype Fix f = Fixed { unFix :: f (Fix f) }
-- data Fix f = Fixed (f (Fix f))
-- GADT notation. f is kind * -> *
-- data Fix f where
--   Fixed :: f (Fix f) -> Fix f
-- unFix :: Fix f -> f (Fix f)
-- unFix (Fixed x) = x

--Implement cata:

-- generalization of foldr
cata :: Functor f => (f s -> s) -> Fix f -> s
cata f = \(Fixed e) -> f $ fmap (cata f) e

--And then use it to write an evaluator for the AST:

type Exp = Fix ExpF

eval :: Exp -> Int
eval = cata $ \x -> case x of
    Mul x y -> (*) x y
    Add x y -> (+) x y
    Const i -> i

--Write a compiler for a simple stack machine:

data Instr = PUSH Int | MUL | ADD deriving Show
compile :: Exp -> [Instr]
compile = cata $ \x -> case x of 
    Mul x y -> x ++ y ++ [MUL]
    Add x y -> x ++ y ++ [ADD]
    Const i -> [PUSH i]

--Here is the test:

mul x y = Fixed $ Mul x y
add x y = Fixed $ Add x y
val x   = Fixed $ Const x
testExpr = mul (val 7) (add (val 3) (val 4))
main = do
    print $ let answer = eval testExpr in case answer == 49 of
        True  -> "You got it right!"
        False -> "Uh oh. Expected 49 as output but got " ++ (show answer)
    putStrLn . unlines . map show $ compile testExpr
