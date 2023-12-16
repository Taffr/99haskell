import Data.List
data Hand = Hand { hand :: [Char], bid :: Int } deriving (Show)

instance Eq Hand where
    (==) (Hand { hand = h1, bid = b1 }) (Hand { hand = h2, bid = b2 }) = cardByCardCompare h1 h2 == EQ

instance Ord Hand where
    compare (Hand { hand = h1, bid = b1 }) (Hand { hand = h2, bid = b2 }) = compareCards h1 h2

compareCards :: [Char] -> [Char] -> Ordering
compareCards h1 h2  
    | diff == EQ = cardByCardCompare h1 h2
    | otherwise = diff
    where
        diff = getScore h1 `compare` getScore h2

cardByCardCompare :: [Char] -> [Char] -> Ordering
cardByCardCompare [] [] = EQ
cardByCardCompare (h1:t1) (h2:t2)
    | h1 == h2 = cardByCardCompare t1 t2
    | otherwise = cardValue h1 `compare` cardValue h2

cardValue :: Char -> Int
cardValue 'A' = 14
cardValue 'K' = 13
cardValue 'Q' = 12
cardValue 'J' = 11
cardValue 'T' = 10
cardValue c = read [c]

getScore :: [Char] -> Int
getScore h 
    | isNOfAKind  5 h = 6
    | isNOfAKind  4 h = 5
    | isFullHouse   h = 4
    | isNOfAKind  3 h = 3
    | isTwoPair     h = 2
    | isNOfAKind  2 h = 1
    | otherwise       = 0

isNOfAKind :: Int -> [Char] -> Bool
isNOfAKind n hand = any (\x -> length x == n) $ group $ sort hand

isFullHouse :: [Char] -> Bool
isFullHouse hand = isNOfAKind 3 hand && isNOfAKind 2 hand

isTwoPair :: [Char] -> Bool
isTwoPair hand = length (filter (isNOfAKind 2) $ group $ sort hand) == 2

main :: IO()
main = do
    input <- readFile "input7.txt"
    let hands = parsePuzzleInput input
    print $ solve hands

parsePuzzleInput :: String -> [Hand]
parsePuzzleInput input = map parseHand $ lines input

parseHand :: String -> Hand
parseHand input = Hand { hand = hand, bid = read bid }
    where
        [hand, bid] = words input

solve :: [Hand] -> Int
solve hands = sum $ zipWith (\r c -> r * bid c) [1..] $ sort hands
