import Data.List (intersect)

day3 = do
    text <- readFile "day3/input"
    putStrLn $ "Part one: " ++ 
        (show $ solve (parse text) (3,1))
    putStrLn $ "Part two: " ++ 
        (show $ product $ map (solve $ parse text) [(1,1),(3,1),(5,1),(7,1),(1,2)])

-- Apply a slope to a coordinate
-- Example: (0,0) `applySlope` (1,3) == (2,6)
applySlope :: (Int, Int) -> (Int, Int) -> (Int, Int)
applySlope (s1, s2) (p1, p2) = ((s1 + p1), (s2 + p2)) 

-- Clip an (x,y) coordinate to a maximum value of x
clip :: Int -> (Int, Int) -> (Int, Int)
clip xm (p1, p2) = (p1 `mod` (xm + 1), p2)

-- Convert the input text to a list of coordinates of trees
parse :: String -> [(Int,  Int)]
parse = doParse 0 0

doParse :: Int -> Int -> String -> [(Int,  Int)]
doParse _ _ [] = []
doParse col row (t:xs) 
    | t == '#'  = (col, row) : (doParse (col+1) row xs)
    | t == '\n' = doParse 0 (row + 1) xs
    | t == '.'  = doParse (col + 1) row xs

-- Solve the puzzle by calculating the following
-- length of (trees `intersect` positions)
-- Where
--      * trees = is a list of all the positions of trees
--      * positions = is a list of al the positions you would visit with a given slope
solve :: [(Int,Int)] -> (Int, Int) ->  Int
solve trees slope = length (positions `intersect` trees)
    where
        maxX = maximum $ map fst trees
        maxY = maximum $ map snd trees
        positions = foldl (\c _ -> (clip maxX $ slope `applySlope` (head c)) : c) [(0,0)] [0..maxY]
        
