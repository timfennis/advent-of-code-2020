import Data.List

day1 = do
    text <- readFile "day1/input"
    let numbers = parseNumbers text
    putStrLn $ "Part 1: " ++ (show $ solvePartOne numbers)
    putStrLn $ "Part 2: " ++ (show $ solvePartTwo numbers)

-- Convert the input String to [Int]
parseNumbers :: String -> [Int]
parseNumbers input = map read (lines input)

-- Part 1 and part 2 of the puzzle
solvePartOne = solve 2020 2
solvePartTwo = solve 2020 3

-- Generate all combinations of size `k` in list `xs`
-- Find the first set where the sum of all elements is `t` and calculates
solve :: Int -> Int -> [Int] -> Int
solve t k xs = head solutions
    where 
        solutions  = filter (/=0) candidates
        candidates = map (solveGroup t) (subsequencesOfSize k xs)

-- Checks if the sum of xs equals s, if true it returns the product of xs, if false it returns 0
solveGroup :: Int -> [Int] -> Int
solveGroup s xs
    | (sum xs) == s = product xs
    | otherwise = 0

-- Deze code is vet hard genakt van stackoverflow
subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = let l = length xs
                          in if (n > l) then []
                             else subsequencesBySize xs !! (l-n)
 where
   subsequencesBySize [] = [[[]]]
   subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                               in zipWith (++)
                                    ([]:next)
                                    ( map (map (x:)) next ++ [[]] )   