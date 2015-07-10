-- ***************************************************************
-- starting out
-- ***************************************************************

doubleMe x = x + x

doubleSmallNumber x = if x > 100
	then x
	else x*2

a = [1,2]
b = [3,4]

-- note ++ walks entire left hand list (a) before it adds b to output list
-- output is [1,2,3,4]
doNothing = a ++ b

-- more efficient is to use cons (:) 
-- which instantaneously adds input to front of list
-- output is [1,2,3,4]
doNothing2 = 1 : 2 : b

-- using !! to extract elements WILL compile
-- but causes a runtime exception if index (3) is out of range
doNothing3 = a !! 3

-- less than & greater than operators applied left to right, lazily
-- short circuits if any element makes operator false
doNothing4 = a > b 

-- head returns first element
-- tail returns [1:]
-- output is 2
doNothing5 = head ( tail a ++ b )

-- other list operations
-- `null` returns True if list is empty
-- `length` returns length of list
-- `reverse` returns reverse of list
-- `maximum`, `minimum`, `sum`, `product`

-- range specfied as [[first element,] first step .. [last element]]
-- note you don't have to specify first or last element 

-- `cycle` turns a list into an infinite list i.e. cycle [1,2,3]
-- `repeat` turns a scalar into an infinite list i.e. repeat 5
-- `replicate` creates a list i.e. replicate 3 10 = [10,10,10]

-- `take` selects first n items from front of list
-- `drop` removes first n itemes from front of list
-- output is [1,2]
doNothing6 = take 2 (a ++ b)

-- returns True if x is an element in the list
doNothing7 x = elem x (a ++ b)

-- list comprehension
-- all numbers between 50 and 100, inclusive, whose modulus of 7 == 3, is even, and is not 80
-- can have multiple inputs as well (i.e. multiple `<-`)
-- output is [52,66,94]
doNothing8 = [ x | x <- [50..100], x `mod` 7 == 3, even x, x /= 80 ]

-- implements `length`
length' :: [t] -> Int
length' array = sum [ 1 | _ <- array ]

-- no such thing as singleton tuple
-- tuple allows mixed types
-- tuple allows enforcement of type size
-- i.e. you can have [[1,2],[3,4],[5,6,7]]
-- but not [(1,2),(3,4),(5,6,7)]

-- for pairs, can use `fst` and `snd` operators to return first, second elements
-- use `zip a b` to create pairs of corresponding pairs from list a and b
-- note zip only zips for as many elements are in the first list

-- which right triangle has integers with all sides equal to or smaller than 10 has a perimeter of n?
-- when n == 24, output is [(6,8,10),(8,6,10)]
triangles :: Int -> [(Int,Int,Int)]
triangles n = [ (a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], a^2 + b^2 == c^2, a + b + c == n ]

-- ***************************************************************
-- types and typeclasses
-- ***************************************************************

-- use :t in ghci to probe type 

-- can sub variables for type when unknown, see length'
-- called 'type variables'

-- basic types
-- Int :: -2^31 <= Int < 2^31
-- Integer :: unbounded
-- Float :: float
-- Double :: double
-- Bool :: True | False
-- Char :: character
-- () :: empty tuple of type ()

-- define class constraints 
-- triangles :: (Eq a) => a -> [(a,a,a)]
-- `Eq` if can be tested for equality 
-- `Ord` if orderable, required for testing max/min
-- `Show` if element can be presented as a string
-- `Bounded` if has upper/lower bound

-- `Num` if numeric
-- `Integral` if integer (Int, Integer)
-- `Floating` if float (Float, Double)
-- use `fromIntegral` to upgrade number from Integral type to Num

-- `Enum` for sequentially ordered, i.e. have a successor and predecessor
-- (), Bool, Char, Ordering, Int, Integer, Float, Double are in this class

-- `Read` turns a string into the appropriate type
-- can use `::` to explicitly give type 

-- ***************************************************************
-- syntax in functions
-- ***************************************************************

-- function can fall through until hits matching case
fib :: (Integral a) => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = factorial (n-1) * n

first :: (a,b,c) -> a
first (x,_,_) = x

second :: (a,b,c) -> b
second (_,x,_) = x

third :: (a,b,c) -> c
third (_,_,x) = x

-- if a pattern match fails, it just moves on to the next item
-- output is [3,7] because [5,6,7] fails pattern matching
xs = [[1,2], [3,4], [5,6,7]]
xs' = [ a + b | [a,b] <- xs ] 

head' :: [a] -> a
head' [] = error "can't take head of empty list"
head' (x:_) = x

-- length using recursive pattern matching
length'' :: [a] -> Int
length'' [] = 0
length'' (_:x) = 1 + length'' x 

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- `@` allows us to keep the entire input list handy
firstLetter :: String -> String
firstLetter "" = "Empty string"
firstLetter phrase@(x:xs) = "First letter of " ++ phrase ++ " is '" ++ [x] ++ "'"

-- switch with catch case
-- `|` called a guard
-- `otherwise` = True
-- note that items must be exactly space aligned to compile
prog :: (RealFloat a) => a -> a -> String
prog weight height 
	| bmi < skinny  = "less than 10"
	| bmi < normal = "less than 20"
	| otherwise     = "moooo"
	where bmi = weight / height ^ 2
	      skinny = 10.0
	      normal = 20.0

-- use `let <bindings> in <expression>` in place of `where`
-- to introduce variables to a local scope
-- (let a = 100; b = 200 in a*b, let foo = "hello"; bar = "world" in foo ++ " " ++ bar)
-- output is (20000,"hello world")

-- use let in list comprehension
-- to calculate list of bmis 
-- filtering out bmis >= 25
calcBMIs :: (RealFloat a) => [(a,a)] -> [a]
calcBMIs xs = [ bmi | (w,h) <- xs, let bmi = w/h^2, bmi < 25 ]

-- case expressions are similar to function pattern matching
-- note the spaces must be exactly the same for cases
-- case is similar to let in that it can be embedded anywhere
head'' :: [a] -> a
head'' xs = case xs of [] -> error "no head of empty list"
                       (x:_) -> x

-- ***************************************************************
-- recursion
-- ***************************************************************

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs)
	| x > rest  = x
	| otherwise = rest 
	where rest = maximum' xs

-- or using lib functions
maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

-- `Num` is not a subclass of `Ord`
-- so we apply both class constraints to the iterator
-- to make sure it steps through recursion properly
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x 
	| n <= 0 = []
	| otherwise = x : replicate' (n-1) x 

-- similar approach 
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
	| n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
repeat' :: a -> [a]
repeat' x = x : repeat' x

elem' :: (Eq a) => a -> [a] -> Bool
elem' e [] = False
elem' e (x:xs)
	| e == x = True
	| otherwise = elem' e xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
	let smallerSorted = quicksort [ a | a <- xs, a <= x ]
	    biggerSorted = quicksort [ a | a <- xs, a > x ]
	in  smallerSorted ++ [x] ++ biggerSorted

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = smallerSorted ++ [x] ++ biggerSorted
	where smallerSorted = quicksort' [ a | a <- xs, a <= x ]
	      biggerSorted = quicksort' [ a | a <- xs, a > x ]

-- ***************************************************************************
-- Curried functions
-- ***************************************************************************

-- every function only takes one parameter
-- if a function is supplied with less than the number of parameters
-- it does a partial application
-- which returns a function that takes the remaining number of arguments
-- ex
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)
-- which is the same as
divideByTen' :: (Floating a) => a -> a
divideByTen' x = x / 10
-- because of partial application in the former 
-- which was applied by using a section `()` around a normally infix function `/`

-- to pass a function, we add `()` around the parameters
-- in this case, `(a -> a)` means a function taking AND returning something of type `a`
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- note the function it takes returns type `c`
-- which is the same type as the output list
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- this function is now a higher order function
-- ex. zipWith' (zipWith' (*)) [[1,2,3],[4,5,6]] [[2,4,6],[8,10,12]]
-- output is [[2,8,18],[32,50,72]]

-- same quicksort using filter instead of list comprehension
quicksort'' :: (Ord a) => [a] -> [a]
quicksort'' [] = []
quicksort'' (x:xs) = smallerSorted ++ [x] ++ biggerSorted
	where smallerSorted = quicksort' (filter (<=x) xs)
	      biggerSorted = quicksort' (filter (>x) xs)

-- find the largest number under 100,000 divisible by 3829
-- note that `head` truncates the filter operation after first element found
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
	where p x = x `mod` 3829 == 0

-- same function using list comprehensions instead of filter
largestDivisible' :: (Integral a) => a
largestDivisible' = head [ n | n <- [100000,99999..], n `mod` 3829 == 0]

-- generate a collatz chain
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
	| even n = n : chain (n `div` 2)
	| odd n  = n : chain (3 * n + 1) 

-- for all numbers between 1 and 100, how many have length > 15
countLengths :: (Integral a) => a
countLengths = sum [ 1 | n <- [1..100], length (chain n) > 15 ]

-- same using map/filter
numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15  

-- folds can be used to reduce
-- this fold uses a lambda, denoted by the `\ params -> func`
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- or more succinctly 
sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (+) 0 xs

-- directionality of fold usually follows direction that 
-- maximizes the efficiency of building the output
-- i.e. lists are easier to construct from right to left (:)

-- function application with `$`
-- ($) :: (a -> b) -> a -> b  
-- lower precendence method to apply functions
-- i.e. `sum ( map sqrt [1..100] )` is equivalent to `sum $ map sqrt [1.100]`

-- function composition with `.`
-- (.) :: (b -> c) -> (a -> b) -> a -> c  
-- allows fusion of functions
-- i.e. `map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]` is the same as 
-- `map (negate . abs) [5,-3,-6,7,-3,2,-19,24]`
-- note this can be chained as many times as needed as `.` is right-associative

-- point free style
-- relying on the currying of functions to reduce explicit variables
