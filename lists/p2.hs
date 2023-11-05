main :: IO()
main = print $ myButLast [1, 2, 3, 4]

myButLast :: [a] -> [a]
myButLast [] = error "Empty list -- myButLast"
myButLast [a] = []
myButLast (a: as) = a : myButLast as

