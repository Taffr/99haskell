import System.IO
import Data.List

data Color = Red | Blue | Green deriving (Show, Eq)
data DiceInfo = DiceInfo Int Color deriving (Show)
newtype Reveal = Reveal [DiceInfo] deriving (Show) 
data Game = Game Int [Reveal] deriving (Show)

getGameNum :: Game -> Int
getGameNum (Game gameNum _) = gameNum

getNumDice :: DiceInfo -> Int
getNumDice (DiceInfo num _) = num

splitOnDelimiter :: Char -> String -> [String]
splitOnDelimiter delimiter = foldr f [[]]
    where
        f c (x:xs)
            | c == delimiter = []:x:xs
            | otherwise = (c:x):xs

readGame :: String -> Game
readGame line = Game gameNum reveals
    where
        reveals = map readReveal $ stripWhiteSpace $ splitOnDelimiter ';' revealsRaw
        droppedPrefix = drop (length "Game ") line
        revealsRaw = drop 1 $ dropWhile (/= ':') droppedPrefix
        gameNum = read $ takeWhile (/= ':') droppedPrefix

readReveal :: String -> Reveal
readReveal line = Reveal diceInfos
    where
        diceInfos = map readDiceInfo $ stripWhiteSpace $ splitOnDelimiter ',' line

stripWhiteSpace :: [String] -> [String]
stripWhiteSpace = filter (not . null)

readDiceInfo :: String -> DiceInfo
readDiceInfo line = DiceInfo num color
    where
        num = read $ head  parts
        color = readColor $ last parts
        parts = words line

readColor :: String -> Color
readColor s 
    | includes "red" = Red
    | includes "blue" = Blue
    | includes "green" = Green
    | otherwise = error $ "Unknown color: " ++ s
    where
        includes = flip isInfixOf s

readPuzzleInput :: String -> IO [Game]
readPuzzleInput path = do
    contents <- readFile path
    let fileLines = lines contents
    return $ map readGame fileLines

-- Actual solution
main :: IO ()
main = do
    games <- readPuzzleInput "input2.txt"
    print $ solve2 games

-- Part 1
solve1 :: [Game] -> Int
solve1 = sum . map getGameNum . filter isGameValid 

isGameValid :: Game -> Bool
isGameValid (Game gameNum reveals) = all isRevealValid reveals
    where
        isRevealValid (Reveal diceInfos) = all isDiceInfoValid diceInfos
        isDiceInfoValid (DiceInfo num color) = case color of
            Red -> num <= 12
            Green -> num <= 13
            Blue -> num <= 14

-- Part 2
data DiceMin = DiceMin Int Int Int deriving (Show)

solve2 :: [Game] -> Int
solve2 = sum . map (getGamePower . getMinDices)

getGamePower :: DiceMin -> Int
getGamePower (DiceMin redMin greenMin blueMin) = redMin * greenMin * blueMin

getMinDices :: Game -> DiceMin
getMinDices (Game gameNum reveals) = DiceMin redMax greenMax blueMax
    where
        redMax   = getMaximumDiceByColor Red reveals
        greenMax = getMaximumDiceByColor Green reveals
        blueMax  = getMaximumDiceByColor Blue reveals

getMaximumDiceByColor :: Color -> [Reveal] -> Int
getMaximumDiceByColor color revs = maximum $ map getNumDice diceInfoOfColor
    where
        diceInfoOfColor = concatMap (getDiceInfoOfColour color) revs
        getDiceInfoOfColour color (Reveal diceInfos) = filter isColour diceInfos
        isColour (DiceInfo _ c) = c == color
