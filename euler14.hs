import Data.List

cltz :: Integer -> [Integer]
cltz 1 = [1]
cltz n | even n = n : cltz (n `div` 2)
       | odd n  = n : cltz (3*n + 1) 

under100over15cltz :: Int
under100over15cltz = length (filter isLong (map cltz [1..100])) where isLong xs = length xs > 15
