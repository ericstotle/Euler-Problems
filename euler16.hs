import Data.List
import Data.Char 

euler16 = sum $ (digits (2^1000)) where digits = map digitToInt . show