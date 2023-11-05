main :: IO()
main = do
  print $ pack ['a', 'a', 'b', 'c', 'd', 'd', 'd', 'e', 'a', 'b', 'b', 'c']
  print $ pack "Simon Tenggren"
  print $ pack ["Simon", "Simon", "AAAA", "Simon", "Test", "Test", "AAAA"]
  print $ pack [1, 1, 1, 1, 4]

pack :: Eq a => [a] -> [[ a ]]
pack = foldr reducer []
  where reducer curr [] = [[ curr ]]
        reducer curr (l:rest)
          | curr == head l = (curr : l) : rest
          | otherwise = [curr] : l : rest
