import Data.List

primes :: [Int]
primes = 2 : filter (null . tail . prime_decomposition) [3,5..]

prime_decomposition :: Int -> [Int]
prime_decomposition n = factor_search n primes
  where
    factor_search n (p:ps)
      | p * p > n       = [n]
      | n `mod` p == 0  = p : factor_search (n `div` p) (p:ps)
      | otherwise       = factor_search n ps

main :: IO ()
main = do
  print $ foldl1 (+) (takeWhile (< 2000000) primes)
