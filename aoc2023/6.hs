main :: IO ()
main = do
    input <- readFile "input6.txt"
    let race = parsePuzzleInput2 input
    print $ solve [race]

data Race = Race { time :: Int, distance :: Int } deriving (Show)

extractNumbers :: String -> [Int]
extractNumbers s = map read droppedPreamble
    where 
        droppedPreamble = drop 1 $ words s

extractNumbers2 :: String -> Int
extractNumbers2 s = read $ filter (/= ' ') $ unwords droppedPreamble
    where 
        droppedPreamble = drop 1 $ words s

parsePuzzleInput :: String -> [Race]
parsePuzzleInput input = map (uncurry Race) tuples
    where 
        [times, distances] = map extractNumbers $ lines input
        tuples = zip times distances

parsePuzzleInput2 :: String -> Race
parsePuzzleInput2 input = Race time distance
    where 
        [time, distance] = map extractNumbers2 $ lines input

solve :: [Race] -> Int
solve rs = product $ map findNumberWins rs

findNumberWins :: Race -> Int
findNumberWins r = length $ filter (\d -> distance r < d) $ getDistancesForMaxTime $ time r

getDistancesForMaxTime :: Int -> [Int]
getDistancesForMaxTime maxTime = map (uncurry (*)) tuples
    where 
        tuples = zip [0 .. maxTime] [maxTime, maxTime - 1 .. 0]
