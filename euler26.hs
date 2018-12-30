import Data.List

primes :: [Integer]
primes = 2 : filter ((== 1) . length . prime_factors) [3,5..]

prime_decomposition :: Integer -> [Integer]
prime_decomposition n = factor_search n primes
  where
    factor_search n (p:ps) | p * p > n       = [n]
                           | n `mod` p == 0  = p : factor_search (n `div` p) (p:ps)
                           | otherwise       = factor_search n ps

prime_factors :: Integer -> [Integer]
prime_factors = nub . concat . group . prime_decomposition

primes_to_1000 :: [Integer]
primes_to_1000 = 2 : filter ((== 1) . length . prime_factors) [3,5..999]

cycle_length :: Integer -> Integer
cycle_length n | n `rem` 2 == 0  = 0
               | n `rem` 5 == 0  = 0
               | otherwise       = head [k | k <- [1..], (10^k-1) `rem` n == 0]

reverse_primes_cycles :: [(Integer,Integer)]
reverse_primes_cycles = zip (reverse primes_to_1000) (map cycle_length (reverse primes_to_1000))

main :: IO ()
main = do
  print $ fst $ head $ filter (\(x,y) -> x == y+1) $ reverse_primes_cycles
