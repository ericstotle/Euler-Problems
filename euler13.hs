main :: IO ()
main = do
  numbers <- fmap (map read . lines) $ readFile "problem13.txt"
  let answer = read (take 10 $ show $ sum numbers) :: Int 
  putStr "Answer: "
  print $ answer
