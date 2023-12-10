main :: IO()
main = print $ elementAt [1,2,3] 2

elementAt :: [a] -> Int -> a
elementAt _ 0 = error "Cannot get elementAt 0 --elementAt"
elementAt [] _ = error "Cannot get element of emptyList --elementAt"
elementAt (x:xs) 1 = x
elementAt (_:xs) n = elementAt xs $ n-1
