module Main where

main :: IO ()
main = do
    input <- readFile "days/7/input.txt"
    let values = parse input
    print $ part1 values
    print $ part2 values

part1 :: [[Int]] -> Int
part1 values = sum $ map (!! 0) (filter (valid [(+), (*)]) values)

part2 :: [[Int]] -> Int
part2 values = sum $ map (!! 0) (filter (valid [(+), (*), concatInt]) values)

-- Utils 
parse :: String -> [[Int]]
parse input = map (map strToInt . words) (lines input)
    where strToInt x = if ':' `elem` x then read (init x) :: Int else read x :: Int

valid :: [Int -> Int -> Int] -> [Int] ->  Bool
valid fs (target:components) = searchPossible fs target 0 components
    where searchPossible fs target total [] = total == target
          searchPossible fs target 0 (x:xs) = searchPossible fs target x xs
          searchPossible fs target total (x:xs) = any (\f -> searchPossible fs target (f total x) xs) fs

concatInt :: (Show a1, Show a2) => a1 -> a2 -> Int
concatInt a b = read $ show a ++ show b :: Int