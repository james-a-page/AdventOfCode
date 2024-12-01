module Main where
import Data.List ( sort )

main :: IO ()
main = do
    input <- readFile "days/1/input.txt"
    print (part2 input)


part1 :: String -> Int
part1 input = 
    let (left,right) = parse (words input) [] [] in
        sum (zipWith absDiff (sort left) (sort right))


part2 :: String -> Int
part2 input = 
    let (left,right) = parse (words input) [] [] in
        productOccurences left right 0


productOccurences :: [Int] -> [Int] -> Int -> Int
productOccurences left right total =
    case left of
        (a:xs) -> productOccurences xs right (total + a * countOcc a right)
        [] -> total


countOcc :: Int -> [Int] -> Int
countOcc x list = 
    length (filter (==x) list)


absDiff :: Num c => c -> c -> c
absDiff n = abs . (-) n


parse :: [String] -> [Int] -> [Int] -> ([Int], [Int])
parse text left right =
    case text of 
        (a:b:xs) -> parse xs (left ++ [read a :: Int]) (right ++ [read b:: Int])
        [] -> (left, right) 
