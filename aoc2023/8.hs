import Prelude hiding (lookup)
import Data.Map (Map, insert, empty, lookup)
import qualified Data.Map as Map

main :: IO ()
main = do
    input <- readFile "input8.txt"
    let ls = lines input
    print $ solve $ parseCamelMap ls

data CamelMap = CamelMap { instructions :: [Char], graph :: Map String (String, String) } deriving (Show)

parseCamelMap :: [String] -> CamelMap
parseCamelMap (instructions : _ : nodes) = CamelMap { instructions = instructions, graph = graph }
  where
    graph = foldr (\node acc -> Map.insert (getKey node) (getTuple node) acc) Map.empty nodes
    getKey s = head $ words $ stripString ['=', ',', '(', ')'] s
    getTuple s = (a, b)
      where
        [a, b] = tail $ words $ stripString ['=', ',', '(', ')'] s

stripString :: [Char] -> String -> String
stripString chars = filter (`notElem` chars) 

solve :: CamelMap -> Int
solve cm = recurse (== "ZZZ") (instructions cm) (graph cm) "AAA"

solve2 :: CamelMap -> Int
solve2 cm = foldr1 lcm $ map (recurse isEndNode (instructions cm) (graph cm)) $ filter isStartingPath $ Map.keys $ graph cm

recurse :: (String -> Bool) -> [Char] -> Map String (String, String) -> String -> Int
recurse p (i:is) m curr
    | p curr = 0
    | otherwise = 1 + recurse p (is ++ [i]) m nextNode
    where 
        nextNode = case Map.lookup curr m of
            Just (l, r) -> if i == 'L' then l else r
            Nothing -> error "bad lookup"

isStartingPath s = last s == 'A'
isEndNode s = last s == 'Z'
