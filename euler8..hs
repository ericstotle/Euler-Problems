pythagorean_triplets =
  [ (a,b,c) | a <- [1..], b <- [1..a], c <- [1..b], a + b + c == 1000, a^2 == b^2 + c^2 ]

main :: IO ()
main = do
  let answer = (\(a,b,c) -> a * b * c) $ head pythagorean_triplets
  print answer
