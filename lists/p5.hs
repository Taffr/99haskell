main :: IO()

main = print $ myReverse [ 1, 2, 3 ]

myReverse :: [a] -> [a]
myReverse = foldl accum []
  where accum = flip (:)
