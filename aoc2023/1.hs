readPuzzleInput :: FilePath -> IO [String]
readPuzzleInput path = do
  contents <- readFile path
  return $ words contents

main :: IO ()
main = do
    strings <- readPuzzleInput "input1.txt"
    print $ solve2 strings

findFirstDigit :: String -> Int

findFirstDigit [] = error "No digit found"
findFirstDigit (x:xs)
    | x `elem` ['0'..'9'] = read [x]
    | otherwise = findFirstDigit xs

findLastDigit :: String -> Int
findLastDigit = findFirstDigit . reverse

solve1 :: [String] -> Int
solve1 = foldr (\s acc -> 10 * findFirstDigit s + findLastDigit s + acc) 0

startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (x:xs) (y:ys)
    | x == y = startsWith xs ys
    | otherwise = False

startsWithDigit :: String -> Bool
startsWithDigit s =
    any (flippedStartsWith s) ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    where
        flippedStartsWith = flip startsWith
    
getStringDigit :: String -> Int
getStringDigit s
    | startsWith "one"   s = 1
    | startsWith "two"   s = 2
    | startsWith "three" s = 3
    | startsWith "four"  s = 4
    | startsWith "five"  s = 5
    | startsWith "six"   s = 6
    | startsWith "seven" s = 7
    | startsWith "eight" s = 8
    | startsWith "nine"  s = 9
    | otherwise = error "Not a digit -- getStringDigit"

findDigits :: String -> [Int]
findDigits [] = []
findDigits (x:xs)
    | x `elem` ['0'..'9'] = read [x] : findDigits xs
    | startsWithDigit (x:xs) = getStringDigit (x:xs) : findDigits xs
    | otherwise = findDigits xs

solve2 :: [String] -> Int
solve2 = foldr (\s acc -> 10 * head (findDigits s) + last (findDigits s) + acc) 0
