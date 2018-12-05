import Data.List
import Data.List.Split
import Data.Char

euler13 = do
  str <- readFile "problem13.txt"
  let answer = map (map fromIntegral) $ chunksOf 50 $ map digitToInt $ concat $ lines str
  let answer1 = map (map toInteger) answer
  let answer2 = sum $ map fromDigits answer1
  print answer2


fromDigits :: [Integer] -> Integer
fromDigits xs = aux xs 0
    where aux [] acc = acc
          aux (x:xs) acc  = aux xs ((acc * 10) + x)

{-

f :: [Integer] -> Integer
f []     = 0
f (x:xs) = x * 10^y ++ f xs where y = length xs
-}

5537376230
