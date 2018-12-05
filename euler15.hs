factorial :: Integer -> Integer
factorial n = product [2..n]


euler15 =  (factorial 40) `div` ((factorial 20) * (factorial 20))
