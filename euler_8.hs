import Data.List
import Data.List.Split
import Data.Char

{-
we want to convert all the content into a string first for formatting reasons
then we want to turn the strinn of numbers into a list of digits

-}

euler_8 = do
   str <- readFile "problem8.txt"
   --let divied = divvy 13 1 $ map digitToInt str
   let du = divvy 13 1 $ map digitToInt $ concat $ lines str
   let answer = maximum $ map product du
   print answer

{-
f :: Int -> [Int] -> [[Int]]
f n (x:xs) =
-}
