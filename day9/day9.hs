import Data.List (tails, subsequences)

day9 = do
    numbers <- (\x -> map read x :: [Int]) <$> lines <$> readFile "day9/input"
    let invalidNumber = findInvalidNumber numbers
        in putStrLn $ "Part one: " ++ (show invalidNumber) ++ "\nPart two: " ++ (show $ solvePartTwo 2 invalidNumber (filter (<invalidNumber) numbers))

findInvalidNumber :: [Int] -> Int
findInvalidNumber ns = last $ head $ filter (not . validateWindow) $ window 26 ns

solvePartTwo :: Int -> Int -> [Int] -> Int
solvePartTwo size t ns
    | (length matches) >= 1 = (maximum $ head matches) + (minimum $ head matches)
    | size >= (length ns) = error "Not found"
    | otherwise = solvePartTwo (size + 1) t ns
    where
        matches = filter ((t==) . sum) $ window size ns

-- 
validateWindow :: [Int] -> Bool
validateWindow xs = (last xs) `elem` subs
    where
        subs = map sum $ subsequencesOfSize 2 (take 25 xs)

-- https://stackoverflow.com/questions/27726739/implementing-an-efficient-sliding-window-algorithm-in-haskell
window m = foldr (zipWith (:)) (repeat []) . take m . tails

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