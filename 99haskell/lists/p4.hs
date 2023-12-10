main :: IO()
main = print $ myLength "Simon"

myLength :: [a] -> Int
myLength = foldr (\_ a -> a + 1) 0
