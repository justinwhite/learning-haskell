{--
funcs are just sugar for lambdas

haskell is an extension to lambda calc
has let expressions, adts, and explicit types

in lambda calc:

variables
expressions
function applications
--}

{--
`forall` is strong because it strictly limits what you can do with the supplied type. Ex. can't do x+1 because we don't know what x is. 
--}

id' x = x

data TrivialType a = TrivialConstructor a deriving Show

-- maybe
data Partial a = Fail | Continue a deriving Show

-- either
data Except e a = Err e | Success a deriving Show

-- cons
data Many a = Some a (Many a) | None deriving Show

class Carrier carrier where
    replaceWith :: (a -> b) -> (carrier a -> carrier b)

instance Carrier TrivialType where
    -- (a -> b) -> (TrivialType a -> TrivialType b) 
    replaceWith f = \x -> case x of 
        TrivialConstructor a -> TrivialConstructor $ f a

instance Carrier Partial where
    -- (a -> b) -> (Partial a -> Partial b)
    replaceWith f = \x -> case x of
        Continue a -> Continue $ f a

        -- phantom type.. 
        -- type doesn't actually show in data constructor
        Fail       -> Fail

instance Carrier (Except e) where
    -- (a -> b) -> (Except e a -> Except e b)
    replaceWith f = \x -> case x of
        Err e -> Err e
        Success a -> Success $ f a

instance Carrier Many where
    -- (a -> b) -> (Many a -> Many b)
    replaceWith f = \x -> case x of
        None -> None
        Some a m -> Some (f a) $ replaceWith f m

main = do
    print $ replaceWith (+1) (TrivialConstructor 1)
    print $ replaceWith (+1) (Continue 1)
    print $ replaceWith (+1) (Fail)
    print $ replaceWith (+1) (Success 1 :: Except String Int)
    print $ replaceWith (+1) (Err "Oh, no!")
    print $ replaceWith (+1) (Some 1 (Some 2 (Some 3 None)))
