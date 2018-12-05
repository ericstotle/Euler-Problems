
import Data.List
import Data.Char
{-

generate all pairs of amicable numbers up to 1000.
the smallest are 220 and 284. the proper divs of 284 are [1,2,4,71,142] those of 200 are [1,2,4,5,10,11,20,22,44,55,110].
 284 == sum [1,2,4,5,10,11,20,22,44,55,110] and 220 == sum [1,2,4,71,142]. 
-}

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

--n = [x | x <- [1.. ((fromIntegral n)/2)], divides x n]

propDivs n = (1:) $ nub $ concat [ [x, div n x] | x <- [2..limit], rem n x == 0 ]
     where limit = (floor.sqrt.fromIntegral) n


amicableNumbers = sum [ a + b | a <- [220..10000], b <- [284..10000], a < b, sum (propDivs b) == a, sum (propDivs a) == b] 

{-
took over 30 minutes though.

p = 2^m * ((2^n-m)+1) 

q = 2^n * ((2^n-m)+1)

r = (2^n+m * ((2^n-m) +1)^2) - 1

where m and n are integers 1 <= m < n. if p, q and r are prime them (2^n)*p*q, (2^n)*r is an amicable pair. 

-}
--here is a faster version

amicable n = let p = sum (propDivs n) in
    p /= n && p < 10000 && n == sum (propDivs p)

main = print $ sum [n | n <- [1..9999], amicable n]