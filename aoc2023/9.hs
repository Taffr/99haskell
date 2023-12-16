main :: IO ()
main = do
    input <- readFile "input9.txt"
    let ls = lines input
        wls = map (map read . words) ls :: [[Int]]
    print $ solve wls

solve :: [[Int]] -> Int
solve = sum . map predict2

predict :: [Int] -> Int
predict = last . extrapolate

predict2 :: [Int] -> Int
predict2 = head . extrapolate2

differences [x, y] = [y-x]
differences (x:y:xs) = (y-x) : differences (y:xs)

extrapolate :: [Int] -> [Int]
extrapolate xs 
    | all (==0) xs = xs
    | otherwise = xs ++ [p1 + p2]
    where
        p1 = last xs
        p2 = last $ extrapolate $ differences xs

extrapolate2 :: [Int] -> [Int]
extrapolate2 xs 
    | all (==0) xs = 0 : xs
    | otherwise = (p1 - p2) : xs
    where
        p1 = head xs
        p2 = head $ extrapolate2 $ differences xs

