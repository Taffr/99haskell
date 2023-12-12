import Data.Map (Map)
import qualified Data.Map as Map

data Card = Card { number :: Int, scratched :: [Int], winning :: [Int] } deriving (Show)

main :: IO ()
main = do
    lines <- lines <$> readFile "input4.txt"
    let cards = zipWith parseCard [1..] lines
    print $ solve2 cards

parseCard :: Int -> String -> Card
parseCard cardNo line = Card { number = cardNo, scratched = map  read $ words scratchedRaw, winning = map read $ words winningRaw }
    where
        droppedBeginning = tail $ dropWhile (/= ':') line
        (scratchedRaw, winningRaw) = (head split, last split)
        split = splitOnDelimiter '|' droppedBeginning

splitOnDelimiter :: Eq a => a -> [a] -> [[a]]
splitOnDelimiter delimiter = foldr f [[]]
    where
        f c (x:xs)
            | c == delimiter = []:x:xs
            | otherwise = (c:x):xs
solve1 :: [Card] -> Int
solve1 cards = sum $ map getScore cards

getScore :: Card -> Int
getScore card
    | 0 == numWinning = 0
    | otherwise = 2 ^ (numWinning - 1)
    where
        numWinning = length $ filter (`elem` winning card) $ scratched card

getNumWinning :: Card -> Int
getNumWinning card = length $ filter (`elem` winning card) $ scratched card

solve2 :: [Card] -> Int
solve2 = sum . spawnCounts


spawnCounts :: [Card] -> [Int]
spawnCounts [] = []
spawnCounts (card : following_cards) = thisCardsSpawns : followingSpawns
    where
        thisCardsSpawns = 1 + sum (take (getNumWinning card) followingSpawns)
        followingSpawns = spawnCounts following_cards
