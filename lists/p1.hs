main:: IO()
main = print $ myLast [1, 2, 3, 4]

myLast :: [a] -> a
myLast [] = error "Empty list -- myLast"
myLast [a] = a
myLast (_:as) = myLast as 
