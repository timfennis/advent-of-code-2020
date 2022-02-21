import Data.List (sort, tails, subsequences, groupBy)


day10 = do
    adapters <- (\x -> 0:(maximum x + 3):x) <$> (\x -> map read x :: [Int]) <$> lines <$> readFile "day10/input"
    print $ sort adapters
    print $ product $ map (conv . length) $ filter (\x -> all (==1) x) $ groupBy (==) $ map (\(a:b:[]) -> b - a ) $ windowed 2 $ sort adapters
    print $ let (a,b) = solvePartOne adapters
            in a * b

solvePartOne :: [Int] -> (Int, Int)
solvePartOne adapters = (ones, threes)
    where 
        ones = length $ filter (1==) diffs
        threes = length $ filter (3==) diffs
        diffs = map (\(a:b:[]) -> b - a ) $ windowed 2 $ sort adapters

conv :: Int -> Int
conv 4 = 7
conv 3 = 4
conv 2 = 2
conv 1 = 1

-- 
windowed m = foldr (zipWith (:)) (repeat []) . take m . tails