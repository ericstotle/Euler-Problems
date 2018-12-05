import Data.List

tNumbers = scanl1 (+) [1..]

euler12 = last $ scanl1 (+) [1..b] 

b = 1 + (length $ takeWhile (<500) $ map numDiv tNumbers)

triangularTest n = if x*(x+1) `div` 2 == n then True else False
                   where x = floor $ sqrt $ fromIntegral (2*n)

exponents n = map length $ group $ primeFactor n

numDiv n = product (map (+1) (exponents n))

factors n = [x|x <- [1..fromIntegral n], n `mod` x == 0]

primeFactor n = iter n primes where
    iter n (p:_) | n < p^2 = [n | n > 1]
    iter n ps@(p:ps') =
        let (d, r) = n `divMod` p
        in if r == 0 then p : iter d ps else iter n ps'

isPrime n | n < 2 = False
isPrime n = all (\p -> n `mod` p /= 0) . takeWhile ((<= n) . (^ 2)) $ primes
primes = 2 : filter isPrime [3..]
