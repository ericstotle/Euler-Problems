import Data.List
import Data.Char

fib :: Integer -> Integer 
fib 1 = 1 
fib n = fib (n-1) + fib (n-2)

-- want the first fib n that equals a thousand digit number. that is length $ fib n == 1000

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

--first1000dig = zip (zip (takeWhile (< 1001) $ map length $ map show fibs) fibs) [1..]

--use this function and scan for the first number at index 1000

first1000dig = print (length (takeWhile (<10^999) fibs))