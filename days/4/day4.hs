module Main where

main :: IO ()
main = do
    input <- readFile "days/4/input.txt"
    let rows = lines input
    print $ part1 rows
    print $ part2 rows

-- Part 1
part1 :: [String] -> Int
part1 grid = sum $ map (checkLine 'X' 0) $ generateLines grid ++ map reverse (generateLines grid)

checkLine :: Char -> Int -> String -> Int
checkLine state count (x:xs) 
    | x == 'S' && x == state = checkLine 'X' (count + 1) xs
    | x == 'A' && x == state = checkLine 'S' count xs
    | x == 'M' && x == state = checkLine 'A' count xs
    | x == 'X' = checkLine 'M' count xs
    | otherwise = checkLine 'X' count xs
checkLine _ count [] = count

generateLines :: [String] -> [String]
generateLines grid =filter ((>=4) . length) (grid ++ leftDiag grid ++ rightDiag grid ++ columns grid)

columns :: [[a]] -> [[a]]
columns grid = [[ grid !! i !! j | i <- [0..n-1]  ] | j <- [0..n-1] ] where n = length grid

leftDiag :: [[a]] -> [[a]]
leftDiag grid =
  [ [grid !! (x + i) !! (y - i) | i <- [0 .. min (n - 1 - x) y]]
  | (x, y) <- leftEdgeIdx n ]
  where
    n = length grid

rightDiag :: [[a]] -> [[a]]
rightDiag grid =
  [ [grid !! (x + i) !! (y + i) | i <- [0 .. min (n - 1 - x) (n - 1 - y)]]
  | (x, y) <- rightEdgeIdx n ]
  where
    n = length grid

leftEdgeIdx :: (Enum b, Num b) => b -> [(b, b)]
leftEdgeIdx n = [(0, c) | c <- [0..n-1]] ++ [(r, n-1) | r <- [1..n-1]]

rightEdgeIdx :: (Enum b, Num b) => b -> [(b, b)]
rightEdgeIdx n = [(0, c) | c <- [0..n-1]] ++ [(r, 0) | r <- [1..n-1]]


-- Part 2
part2 :: [String] -> Int
part2 grid = length $ filter (checkIfXMAS grid) $ getXMidIdx grid

checkIfXMAS :: [String] -> (Int, Int) -> Bool
checkIfXMAS grid (x,y) = 
    let rightDiag = [grid !! (x - 1) !! (y - 1),grid !! (x + 1) !! (y + 1)] in
        let leftDiag = [grid !! (x + 1) !! (y - 1),grid !! (x - 1) !! (y + 1)] in
            (rightDiag == "MS" || rightDiag == "SM") && (leftDiag == "MS" || leftDiag == "SM") 


getXMidIdx :: [String] -> [(Int, Int)]
getXMidIdx grid = [(i,j) | i<-[1..n-2], j<-[1..n-2], grid !! i !! j == 'A' ] where n = length grid