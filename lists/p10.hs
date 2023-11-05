main :: IO()

main = do
  print $ encode "aaaabccaadeeee"


encode :: Eq a => [a] -> [(Int, a)]
encode = map (\p -> (length p, head p)) . pack

-- Solution to p9
pack :: Eq a => [a] -> [[ a ]]
pack = foldr reducer []
  where reducer curr [] = [[ curr ]]
        reducer curr (l:rest)
          | curr == head l = (curr : l) : rest
          | otherwise = [curr] : l : rest
