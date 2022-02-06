import Data.List ((\\))

day5 = do
    text <- readFile "day5/input"
    print $ maximum $ map (uniqueId . parseCode) $ parse text
    print $ findSeat $ map (uniqueId . parseCode) $ parse text

parse = parse' ""

parse' :: String -> String -> [String]
parse' b [] = [b]
parse' b (x:xs)
    | x == '\n' = b : (parse' "" xs)
    | otherwise = parse' (b ++ [x]) xs

uniqueId (r,s) = r * 8 + s 

parseCode s = (rowToInt (take 7 s), chairToInt (drop 7 s))

findSeat :: [Int] -> Int
findSeat xs = head $ [min..max] \\ xs
    where
        min = minimum xs
        max = maximum xs

rowToInt :: String -> Int
rowToInt = stringToInt 'F' 'B'

chairToInt :: String -> Int
chairToInt = stringToInt 'L' 'R'

stringToInt :: Char -> Char -> String -> Int
stringToInt l u [n]
    | n == l = 0
    | n == u = 1
stringToInt l u (n:xs)
    | n == l = 0 + (stringToInt l u xs)
    | n == u = (2 ^ (length xs)) + (stringToInt l u xs)