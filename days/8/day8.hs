module Main where
import Data.List (permutations, tails)
import Data.Map (Map, empty, insertWith, toList)
import Data.Set (Set, empty, insert, union, filter, size, fromList)

main :: IO ()
main = do
    input <- readFile "days/8/input.txt"
    let grid = lines input
    print $ part1 grid
    print $ part2 grid


-- Part 1
part1 :: [String] -> Int
part1 grid = countAntinodes getAntinodes (length grid) grid

getAntinodes :: Set (Int,Int) -> [((Int,Int), (Int,Int))] -> Set (Int,Int)
getAntinodes set [] = set
getAntinodes set ((p1,p2):ps) = getAntinodes (set `union` fromList (findAntinodes p1 p2)) ps
    where
        findAntinodes (x1,y1) (x2,y2)
            | x1 <= x2 && y1 <= y2 = [(x1 - xDiff, y1 - yDiff), (x2 + xDiff, y2 + yDiff)]
            | x1 >= x2 && y1 <= y2 = [(x1 + xDiff, y1 - yDiff), (x2 - xDiff, y2 + yDiff)]
            | x1 <= x2 && y1 >= y2 = [(x1 - xDiff, y1 + yDiff), (x2 + xDiff, y2 - yDiff)]
            | x1 >= x2 && y1 >= y2 = [(x1 + xDiff, y1 + yDiff), (x2 - xDiff, y2 - yDiff)]
                where
                    xDiff = absDiff x1 x2
                    yDiff = absDiff y1 y2

-- Part 2
part2 :: [String] -> Int
part2 grid = countAntinodes getAntinodesWithResonance (length grid) grid
    where n = length grid

getAntinodesWithResonance :: Set (Int,Int) -> [((Int,Int), (Int,Int))] -> Set (Int,Int)
getAntinodesWithResonance set [] = set
getAntinodesWithResonance set ((p1,p2):ps) = getAntinodesWithResonance (set `union` fromList (findAntinodesWithResonance 50 p1 p2)) ps
    where 
        findAntinodesWithResonance r (x1,y1) (x2,y2)
            | x1 <= x2 && y1 <= y2 = [(x1 - (xDiff * m), y1 - (yDiff * m)) | m <- [0..r]] ++ [(x2 + (xDiff * m), y2 + (yDiff * m)) | m <- [0..r]]
            | x1 >= x2 && y1 <= y2 = [(x1 + (xDiff * m), y1 - (yDiff * m)) | m <- [0..r]] ++ [(x2 - (xDiff * m), y2 + (yDiff * m)) | m <- [0..r]]
            | x1 <= x2 && y1 >= y2 = [(x1 - (xDiff * m), y1 + (yDiff * m)) | m <- [0..r]] ++ [(x2 + (xDiff * m), y2 - (yDiff * m)) | m <- [0..r]]
            | x1 >= x2 && y1 >= y2 = [(x1 + (xDiff * m), y1 + (yDiff * m)) | m <- [0..r]] ++ [(x2 - (xDiff * m), y2 - (yDiff * m)) | m <- [0..r]]
                where
                    xDiff = absDiff x1 x2
                    yDiff = absDiff y1 y2



-- Utils
countAntinodes :: (Set (Int, Int) -> [((Int, Int), (Int, Int))] -> Set (Int, Int)) -> Int -> [[Char]] -> Int
countAntinodes f n grid = size $ Data.Set.filter (inBox n) $ foldl f Data.Set.empty $ map (\(_,l) -> pairs l) $ toList (collectIndexes (0,0) Data.Map.empty grid)

collectIndexes :: (Int, Int) -> Map Char [(Int,Int)] -> [[Char]] -> Map Char [(Int,Int)]
collectIndexes (i,j) map [] = map
collectIndexes (i,j) map (x:xs) = collectIndexes (i+1, j) (collectIndexesRow (i,0) map x) xs
    where
        collectIndexesRow :: (Int, Int)-> Map Char [(Int,Int)] -> [Char] -> Map Char [(Int,Int)]
        collectIndexesRow (i,j) map (x:xs) = if x /= '.' then collectIndexesRow (i,j+1) (insertWith (++) x [(i,j)] map) xs else collectIndexesRow (i,j+1) map xs
        collectIndexesRow _ map [] = map

pairs :: [b] -> [(b, b)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

inBox :: (Ord a, Num a) => a -> (a, a) -> Bool
inBox n (x,y) = x < n && x >= 0 && y < n && y >= 0

absDiff :: Num c => c -> c -> c
absDiff n = abs . (-) n