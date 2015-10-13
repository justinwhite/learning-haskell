-- 13 august 
--
-- ******************************************************************
--  type signatures
-- ******************************************************************

def foo(s):
    return a * "bar"

>> foo(3)
>> "barbarbar"

foo :: Int -> String
foo a = a * "bar"

foo :: Integral a => a -> String
foo a = a * "bar"

type Temp = Float
type USD = Float
a = 13.5
b = 100.3
-- compiler won't catch this, even tho wrong
c = a + b 
-- use data to enforce this strictness

data Currency = USD Dollars Cents | CAD Dollars | EUR .. 
-- this can fail due to namespace clashing
-- because record type actually defines functions
-- and in this case `dollars` has been redefined
data Currency = USD { dollars :: Int, cents :: Int }, CAD { dollars :: Int }

-- if type wasn't defined with record, have to use pattern matching
-- if it was, then we can use the accessor functions like `dollar`

-- imports everything but `Map`
import Data.List hiding (Map)

-- ******************************************************************
--  types
-- ******************************************************************

using `type`
is just an alias

-- ******************************************************************
--  algebraeic data types
-- ******************************************************************

using `data`

-- ******************************************************************
--  newtypes
-- ******************************************************************

combination of type and algebraeic type
no performance penalty because fully checked at compile time
** but only because newtypes can only have one field ** 
essentially wrapping newtype with external type for protection
whereas `data` will have a penalty
safety of algebraeic type, performance of type

newtype Temp = Temp Float
newtype USD = USD Float

-- function with zero inputs is a constant
-- specifying type (::) isn't necessary
a :: Temp
a = Temp 50.5

a = Temp 50.5 
b = USD 101.07
-- this won't work because newtype is actually a new type
c = a + b
-- this won't work because no function defined for it
c = a + a


newtype Temp = Temp Float deriving Numeric
newtype USD = USD Float deriving Numeric
a = Temp 50.5 
b = USD 101.07
-- now this will work 
c = a + a
-- this still won't work
c = a + b

-- ******************************************************************
--
--  type classes
-- ******************************************************************

data Currency = USD Dollars Cents
instance Numeric Currency where
    (Currency d1 c1) + (Currency d2 c2) = Currency (d1 + d2) (c1 + c2)
instance Show Currency where
    show (Currency d c) = "$" ++ (show d) ++ "." ++ (show c)

-- ******************************************************************
--  pattern matching
-- ******************************************************************

-- ******************************************************************
--  misc
-- ******************************************************************
Int is 64 bit





-- 24 august 
--
-- ******************************************************************
--  lambdas
-- ******************************************************************

runtime error occurs if pattern matching fails
i.e. 
map (\(a,b) -> a + b) [(1,2,3),(4,5,6)]

example of using lambda instead of explicit recursion? 

-- ******************************************************************
--  functors
-- ******************************************************************

typeclass for things that can be mapped over
mapping from set A to set B
<$> synonymous with `fmap`, `lift`
"lifting" from A to B

instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub) 

-- ******************************************************************
--  applicatives
-- ******************************************************************
<*> is an applicative
<*> :: f (a -> b) -> f a -> f b

(*) <$> Just 5 <*> Just 7
lift `*` into Just 5
which produces (Just 5*)
and then is applied to Just 7
resulting in `Just 35`

`>>=` is a bind
>>= :: m a -> (a -> m b) -> m b
anther way to `fmap` into a monad

`return` is equivalent of apply

try using a bind if the `do` notation doesn't work

-- ******************************************************************
--  monads
-- ******************************************************************

