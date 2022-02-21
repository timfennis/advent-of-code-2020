import Data.List ((\\))

day5 = do
    text <- lines <$> readFile "day5/input"
    putStrLn $ "Part one: " ++ (show $ maximum $ map (uniqueId . parseCode) text)
    putStrLn $ "Part two: " ++ (show $ findSeat $ map (uniqueId . parseCode) text)

-- Convert a seat number to a unique id
uniqueId :: (Int, Int) -> Int
uniqueId (r,s) = r * 8 + s 

parseCode :: String -> (Int, Int)
parseCode s = (rowToInt (take 7 s), chairToInt (drop 7 s))

-- Find the seat that is not assigned to anyone by calculating the difference between all seats and xs
findSeat :: [Int] -> Int
findSeat xs = let allSeats = [minimum xs..maximum xs]
    in head $ allSeats \\ xs
        
-- Utilities for parsing strings of essentially binary code to integers
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