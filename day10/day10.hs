import Data.List (sort, tails, subsequences, groupBy)


day10 = do
    adapters <- (\x -> 0:(maximum x + 3):x) <$> (\x -> map read x :: [Int]) <$> lines <$> readFile "day10/input"
    putStrLn $ "Part one: " ++ (show $ solvePartOne adapters)
    putStrLn $ "Part two: " ++ (show $ solvePartTwo adapters)

solvePartOne :: [Int] -> Int
solvePartOne adapters = ones * threes
    where 
        ones = length $ filter (1==) diffs
        threes = length $ filter (3==) diffs
        diffs = map (\(a:b:[]) -> b - a ) $ windowed 2 $ sort adapters

solvePartTwo :: [Int] -> Int
solvePartTwo = product . map (conv . length) . filter (\x -> all (==1) x) . groupBy (==) . map (\(a:b:[]) -> b - a ) . windowed 2 . sort

conv :: Int -> Int
conv 4 = 7
conv 3 = 4
conv 2 = 2
conv 1 = 1

-- 
windowed m = foldr (zipWith (:)) (repeat []) . take m . tails