{-
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
-}

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

lowestdiv :: Integer -> Integer
lowestdiv n = lowestdivfromk 2 n

lowestdivfromk :: Integer -> Integer -> Integer
lowestdivfromk k n | divides k n = k
                   | k^2 > n     = n
                   | otherwise   = lowestdivfromk (k + 1) n

lowestprimediv :: Integer -> Integer
lowestprimediv n = lowestprimedivf primes1 n

lowestprimedivf :: [Integer] -> Integer -> Integer
lowestprimedivf (p:ps) n | rem n p == 0 = p
                         | p^2 > n      = n
                         | otherwise    = lowestprimedivf ps n

primes1 :: [Integer]
primes1 = 2 : filter prime [3..]

prime :: Integer -> Bool
prime n | n < 1     = error "not pos int"
        | n == 1    = False
        | otherwise = lowestprimediv n == n

euler10 = sum $ filter prime [2..2000000]
