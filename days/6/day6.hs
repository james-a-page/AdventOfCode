module Main where
import Data.List (transpose, elemIndex, findIndex)
import Data.Maybe (fromMaybe)
import Control.Arrow (ArrowLoop(loop))

main :: IO ()
main = do
    input <- readFile "days/6/input.txt"
    let grid = lines input
    print $ part1 grid
    print $ part2 grid


-- Part 1
part1 :: [String] -> Int
part1 grid = sum (map (countOccurrences 'X') (markPath $ rotateRight grid)) + 1

markPath :: [String] -> [String]
markPath grid =
    if guardOnEdge grid then grid
    else markPath $ rotateLeft $ map (\r -> if '^' `elem` r then walkLine "" r else r) grid

walkLine :: String -> String -> String
walkLine prev ('^':'#':xs) = prev ++ "^#" ++ xs
walkLine prev ('^':y:xs) = walkLine (prev ++ "X") ('^':xs)
walkLine prev (x:xs) = walkLine (prev ++ [x]) xs
walkLine prev [] = prev

guardOnEdge :: [String] -> Bool
guardOnEdge grid = elem '^' (head grid) ||
                    elem '^' (last grid) ||
                    elem '^' (head $ transpose grid) ||
                    elem '^' (last $ transpose grid)

-- Part 2
part2 :: [String] -> Int
part2 grid = 0 -- TODO


updateDirection :: Char -> Char
updateDirection 'u' = 'r'
updateDirection 'r' = 'd'
updateDirection 'd' = 'l'
updateDirection 'l' = 'u'

-- Utils
rotateLeft :: [[a]] -> [[a]]
rotateLeft = reverse . transpose

rotateRight :: [[a]] -> [[a]]
rotateRight = transpose . reverse

countOccurrences :: (Ord a) => a -> [a] -> Int
countOccurrences x list =
    length (filter (==x) list)