import Data.List

main = do
    text <- readFile "input"
    let numbers = parseNumbers text
    print $ solve $ subsequencesOfSize 2 numbers
    print $ solve $ subsequencesOfSize 3 numbers

solve :: [[Int]] -> Int
solve xs = head $ filter (/=0) $ map (solvePair 2020) xs

solvePair :: Int -> [Int] -> Int
solvePair s xs
    | (sum xs) == s = product xs
    | otherwise = 0

parseNumbers :: String -> [Int]
parseNumbers input = map read (lines input)

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