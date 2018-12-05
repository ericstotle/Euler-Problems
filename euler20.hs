import Data.List
import Data.Char

factorial n = product [1..n]

euler20 = sum $ map digitToInt $ show $ factorial 100