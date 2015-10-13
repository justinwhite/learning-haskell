cmp :: [Int] -> [Int] -> [Int]
cmp l1 l2
    | l1' == 0 = l2
    | l2' == 0 = l1
    | l1' < l2' = l1
    | otherwise = l2
    where l1' = sum l1
          l2' = sum l2

{-- 
 - given a sorted (largest to smallest) 
 - list of possible coins and a value
 - they should sum to, generate the 
 - smallest number of coins required to 
 - sum to the value
 -
 - ex. count [25,15,1] 30 -> [0,2,0]
 --}
count :: [Int] -> Int -> [Int]
count [] v = []
count (x:xs) v = cmp with without
    where n = v `quot` x
          with = n : count xs (v - n * x)
          without = 0 : count xs v 
