import Data.List
data Part = Part {
    row :: Int,
    cols :: (Int, Int),
    partValue :: Int
} deriving (Show)

data Symbol = Symbol {
    position :: (Int, Int),
    character :: Char
} deriving (Show)

data Schematic = Schematic {
    parts :: [Part],
    symbols :: [Symbol]
} deriving (Show)

readSchematic :: [String] -> Schematic
readSchematic [] = Schematic [] []
readSchematic lines = Schematic allParts allSymbols
    where
        allParts = concat $ zipWith readParts [0..] lines
        allSymbols = concat $ zipWith readSymbols [0..] lines
        
   
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c || c == '.')

readSymbols :: Int -> String -> [Symbol]
readSymbols lineNo line = map (\(p, c) -> Symbol { position = (lineNo, p), character = c }) symbolCols
    where
        symbolCols = filter (\(pos,c) -> isSymbol c) $ zipWithCol line

readParts :: Int -> String -> [Part]
readParts lineNo = map (\(start, valueAsString) -> Part lineNo (start, start - 1 + length valueAsString) (read valueAsString)) . groupDigitCols

groupDigitCols :: String -> [(Int, String)]
groupDigitCols line = map flatten $ inner initial
    where
        initial = digitCols line

        flatten :: [(Int, Char)] -> (Int, String)
        flatten xs = foldl (\(s, sc) (i, c) -> (s, sc ++ [c])) seed rest
            where
                rest = tail xs
                seed = (s, sc : "")
                (s, sc) = head xs

        inner :: [(Int, Char)] -> [[(Int, Char)]]
        inner [] = []
        inner xs = bunch : inner (drop (length bunch) xs)
           where bunch = takeWhileIncreasingByOne xs

takeWhileIncreasingByOne :: [(Int, Char)] -> [(Int, Char)]
takeWhileIncreasingByOne [] = []
takeWhileIncreasingByOne [x] = [x]
takeWhileIncreasingByOne (x:y:xs)
    | fst x + 1 == fst y = x : takeWhileIncreasingByOne (y:xs)
    | otherwise = [x]

digitCols line = filter (\(_,c) -> isDigit c) $ zipWithCol line
zipWithCol :: String -> [(Int, Char)]
zipWithCol = zip [0..] 

readPuzzleInput :: String -> IO Schematic
readPuzzleInput path = do
    contents <- readFile path
    let fileLines = lines contents
    return $ readSchematic fileLines

main :: IO ()
main = do
    schematic <- readPuzzleInput "input3.txt"
    print $ solve2 schematic

solve1 schematic = sum $ map partValue $ filter (anyAdjacent ss) ps
    where
        ps = parts schematic
        ss = symbols schematic 

solve2 :: Schematic -> Int
solve2 schematic = sum $ map (\pl -> partValue (head pl) * partValue (last pl)) withTwoNeighbors
    where
        withTwoNeighbors = filter (\pl -> length pl == 2) $ map (getNeighbors ps) potentialGears
        potentialGears = filter (\s -> character s == '*') $ symbols schematic
        ps = parts schematic

getNeighbors :: [Part] -> Symbol -> [Part]
getNeighbors ps symbol = filter (\p -> inSurrounding symbol p) ps

anyAdjacent :: [Symbol] -> Part -> Bool
anyAdjacent symbols part = any (\s -> inSurrounding s part) symbols

inSurrounding :: Symbol -> Part -> Bool
inSurrounding symbol part = position symbol `elem` surrounding part

surrounding :: Part -> [(Int, Int)]
surrounding part = topSurrounding ++ bottomSurround ++ leftSide ++ rightSide 
    where
        topSurrounding = map (rp - 1,) [startCol - 1.. endCol + 1]
        bottomSurround = map (rp + 1,) [startCol - 1.. endCol + 1]
        leftSide = [(rp, startCol - 1)]
        rightSide = [(rp, endCol + 1)]

        (startCol, endCol) = cols part
        rp = row part
