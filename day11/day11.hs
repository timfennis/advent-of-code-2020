import Data.List (intercalate)

day11 = do
    text <- readFile "day11/input"
    -- print $ zip [0..(length text)] text
    -- putStrLn $ take 97 $ repeat '-'
    print $ length $ filter (=='#') $ findEq text ""
    -- putStrLn $ take 15 $ repeat '-'
    -- putStrLn $ simulateOneStep $ simulateOneStep text
    -- print $ (simulateOneStep text == text)
    
simulateOneStep :: String -> String
simulateOneStep state = intercalate "\n" $ chunksOf width $ foldr (\(offset, char) acc -> 
        let nbs = map (strippedState !!) $ neighbours offset
        in (case char of '#' -> if flipOff nbs then 'L' else '#'
                         'L' -> if flipOn nbs then '#' else 'L'
                         '.' -> '.'):acc) [] stateWithOffset
    where
        width           = (length $ head $ lines state)
        height          = length $ lines state
        toOffset        = pointToOffset width
        toPoint         = offsetToPoint width
        stateWithOffset = zip [0..(length strippedState - 1)] strippedState
        strippedState   = filter (/='\n') state
        neighbours      = map toOffset . neighboursInBound width height . toPoint

-- Find an equilibrium state
findEq :: String -> String -> String
findEq cur prev
    | cur == prev = cur
    | otherwise = findEq (simulateOneStep cur) cur

-- Rules of the simulation
isOccupied :: Char -> Bool
isOccupied '#' = True
isOccupied _   = False

flipOn :: [Char] -> Bool
flipOn = all (not . isOccupied)

flipOff :: [Char] -> Bool
flipOff cs = (length $ filter isOccupied cs) >= 4

-- Helper functions to convert between axis systems
type Point = (Int, Int)

offsetToPoint :: Int -> Int -> Point
offsetToPoint w o = (o `mod` w, o `div` w)

pointToOffset :: Int -> Point -> Int
pointToOffset w (x,y)
    | x >= w = error "Out of bounds"
    | otherwise = (y * w) + x

neighboursInBound :: Int -> Int -> Point -> [Point]
neighboursInBound w h (x,y)
    | w == x || h == y = error "Out of bounds"
    | otherwise = filter (isInBounds w h) (neighbours (x,y))

neighbours :: Point -> [Point]
neighbours (x,y)
    | x < 0 || y < 0 = error "Out of bounds" 
    | otherwise = [(x-1, y-1), (x, y-1), (x+1, y-1),
                   (x-1, y), (x+1, y),
                   (x-1, y+1), (x, y+1), (x+1, y+1)]

isInBounds :: Int -> Int -> Point -> Bool
isInBounds w h (x,y) = x >= 0 && y >= 0 && x < w && y < h

-- Stackoverflow

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
  | n > 0 = (take n l) : (chunksOf n (drop n l))
  | otherwise = error "Negative or zero n"
