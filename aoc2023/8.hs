import Prelude hiding (lookup)
import Data.Map (Map, insert, empty, lookup)
import qualified Data.Map as Map

main :: IO ()
main = do
    input <- readFile "1.txt"
    let ls = lines input
    print $ solve2 $ parseCamelMap ls

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
solve cm = recurse "ZZZ" (instructions cm) "AAA"
    where
        recurse :: String -> [Char] -> String -> Int
        recurse dest (i:is) curr
            | dest == curr = 0
            | otherwise = 1 + recurse dest nextInstructions nextNode
            where 
                nextNode = case Map.lookup curr (graph cm) of
                    Just (l, r) -> if i == 'L' then l else r
                    Nothing -> error "bad lookup"
                nextInstructions :: [Char]
                nextInstructions = is ++ [i]

isStartingPath s = last s == 'A'
isEndNode s = last s == 'Z'

solve2 :: CamelMap -> Int
solve2 cm = recurse (instructions cm) $ filter isStartingPath $ Map.keys $ graph cm
    where
        recurse :: [Char] -> [String] -> Int
        recurse (i:is) currNodes
            | (not . all isEndNode) currNodes = 1 + recurse (is ++ [i]) nextNodes
            | otherwise = 0
            where 
                nextNodes = map nextNode currNodes
                nextNode curr = case Map.lookup curr (graph cm) of
                    Just (l, r) -> if i == 'L' then l else r
                    Nothing -> error "bad lookup"
