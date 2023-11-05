main :: IO()

main = do
  print $ compress [1,1,1]
  print $ compress [1,2,2,3,4,4,4,5]
  print $ compress "aaabbbcccdeee"

compress :: Eq a => [a] -> [a]
compress = foldr accIfNotEq []
  where accIfNotEq a [] = [a]
        accIfNotEq current accumulator
          | current == head accumulator = accumulator
          | otherwise = current: accumulator
