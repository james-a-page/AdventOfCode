module Main where
import Data.List ( delete ) 

main :: IO ()
main = do
    input <- readFile "days/2/input.txt"
    let rows = parse input
    print $ part1 rows
    print $ part2 rows

-- Part 1
part1 :: [[Int]] -> Int
part1 rows = length $ filter id (map checkSeq (rows))

checkSeq :: (Ord a, Num a) => [a] -> Bool
checkSeq row = checkCond (<) row || checkCond (>) row

checkCond :: (Ord a, Num a) => (a -> a -> Bool) -> [a] -> Bool
checkCond _ [] = True
checkCond _ [x] = True
checkCond f (x:y:xs) = f x y && (absDiff x y <= 3) && checkCond f (y:xs)

-- Part 2
part2 :: [[Int]] -> Int
part2 rows = length $ filter (>=1) $ map (part1 . removeOne) rows

removeOne :: Eq a => [a] -> [[a]]
removeOne []     = []
removeOne (x:xs) = xs : map (x :) (removeOne xs)


-- Utils
parse :: String -> [[Int]]
parse x = map (map strToInt . words) $ lines x

strToInt :: String -> Int
strToInt x = read x :: Int 

absDiff :: Num c => c -> c -> c
absDiff n = abs . (-) n