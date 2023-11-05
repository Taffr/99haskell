import Data.List
main :: IO()

main = do
  print $ length $ queens 8
  print $ head $ queens 8

queens :: Int -> [[Int]]
queens n = filter isValid $ permutations [1..n]

isValid :: [Int] -> Bool
isValid []      = True
isValid (q:qs)  = not $ sameDiag q qs && isValid qs
  where sameDiag try qs = any (\(colDist, q) -> abs (try - q) == colDist) $ zip [1..] qs

