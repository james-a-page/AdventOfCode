module Main where
import Data.List (sort)

main :: IO ()
main = do
    input <- readFile "days/1/input.txt"
    let (left, right) = parse (words input) [] [] 
    print (part2 left right)

-- part1 :: String -> Int
part1 :: (Num a, Ord a) => [a] -> [a] -> a
part1 left right = 
    sum (zipWith absDiff (sort left) (sort right))

part2 :: [Int] -> [Int] -> Int
part2 left right = 
    productOccurences left right 0

productOccurences :: [Int] -> [Int] -> Int -> Int
productOccurences left right total =
    case left of
        (a:xs) -> productOccurences xs right (total + a * countOccurrences a right)
        [] -> total

countOccurrences :: (Ord a) => a -> [a] -> Int
countOccurrences x list = 
    length (filter (==x) list)

absDiff :: Num c => c -> c -> c
absDiff n = abs . (-) n

parse :: [String] -> [Int] -> [Int] -> ([Int], [Int])
parse text left right =
    case text of 
        (a:b:xs) -> parse xs (left ++ [read a :: Int]) (right ++ [read b:: Int])
        [] -> (left, right) 
