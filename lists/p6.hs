main :: IO()
main = do
  print $ isPalindrome "naturirutan"
  print $ isPalindrome [1,2,1]
  print $ isPalindrome [3,2,1]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [a] = True
isPalindrome (f:xs) = f == l && isPalindrome (init xs)
  where l = last xs
