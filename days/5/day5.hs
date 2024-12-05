module Main where
import Data.List (sortBy)
import Data.List.NonEmpty (sortWith)
import GHC.Utils.Misc

main :: IO ()
main = do
    input <- readFile "days/5/input.txt"
    let (rules, lists) = parse $ lines input
    print $ part1 rules lists
    print $ part2 rules lists

-- Part 1
part1 :: [(Int,Int)] -> [[Int]] -> Int
part1 rules lists = sum $ map getMidValue (filter (checkOrdering rules []) lists)

checkOrdering :: [(Int,Int)] -> [Int] -> [Int] -> Bool
checkOrdering rules prev (x:xs) = not (checkAfterBreak rules x xs) && not (checkPriorBreak rules x prev) && checkOrdering rules (prev++[x]) xs
checkOrdering _ _ [] = True

checkAfterBreak :: [(Int, Int)] -> Int -> [Int] -> Bool
checkAfterBreak rules x = any (\y -> (y,x) `elem` rules)

checkPriorBreak :: [(Int, Int)] -> Int -> [Int] -> Bool
checkPriorBreak rules x = any (\y -> (x,y) `elem` rules)

getMidValue :: [Int] -> Int
getMidValue list = list !! (length list `div` 2)

-- Part 2
part2 :: [(Int,Int)] -> [[Int]] -> Int
part2 rules lists = sum $ map getMidValue $ fixOrder rules (filter (not . checkOrdering rules []) lists) 

fixOrder :: [(Int,Int)] -> [[Int]] -> [[Int]]
fixOrder rules = map (sortWithRules rules)

sortWithRules :: [(Int, Int)] -> [Int] -> [Int]
sortWithRules rules = sortBy rulesComparison
    where 
        rulesComparison a b
            | (a,b) `elem` rules = LT
            | (b,a) `elem` rules = GT
            | otherwise = EQ


-- Utils
-- Lines -> (List of rules, List of orderings)
parse :: [String] -> ([(Int,Int)], [[Int]])
parse = foldl parseLine ([],[])


parseLine :: ([(Int,Int)], [[Int]]) -> String -> ([(Int,Int)], [[Int]])
parseLine (currRules, currOrderings) line
    | '|' `elem` line = (currRules ++ [(strToInt $ head (split '|' line), strToInt $ last (split '|' line))], currOrderings)
    | ',' `elem` line = (currRules, currOrderings ++ [map strToInt $ split ',' line])
    | otherwise = (currRules, currOrderings)

strToInt :: String -> Int
strToInt x = read x :: Int