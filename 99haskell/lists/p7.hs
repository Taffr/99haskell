main :: IO()
main = do
  print $ myFlatten (Elem 5)
  print $ myFlatten (List [ Elem 5, Elem 3])
  print $ myFlatten (List [ Elem 1, List [ Elem 5, List [Elem 3, Elem 9 ] ], List [], Elem 10 ])

data NestedList a = Elem a | List [NestedList a]

myFlatten :: Show a => NestedList a -> [a]
myFlatten (List []) = []
myFlatten (Elem a) = [a]
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)
