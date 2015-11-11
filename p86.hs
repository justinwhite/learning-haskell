{--
6,5,3 = 10
2060 with int dim up to m^3
m=100 -> 2060
m=99  -> 1975
find m that first exceeds 1m
--}

import Data.Set (toList, fromList)

mag x y z = sqrt $ x^2 + y^2 + z^2

trunc x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)
    where n = 4

isInt x = trunc x == (fromIntegral $ round x)

shortest x y z a = filter (mag x y z <=) $ map sqrt [x',y',z']
    where
        x' = 2*a^2 - 2*x*a + x^2 + y^2 + z^2 
        y' = 2*a^2 - 2*y*a + x^2 + y^2 + z^2 
        z' = 2*a^2 - 2*z*a + x^2 + y^2 + z^2 

uniq = toList . fromList

explore x y z = filter isInt $ uniq $ concat $ map (shortest x y z) xs
    where 
        largest = maximum [x,y,z]
        xs = [0, dx .. largest]

dx = 0.0001

brute m = length $ uniq $ map trunc $ concat $ map short cs
    where
        xs = [1..m]
        ys = [1..m]
        zs = [1..m]
        cs = [ (x,y,z) | x<-xs, y<-ys, z<-zs ]

short (x,y,z) = explore x y z
