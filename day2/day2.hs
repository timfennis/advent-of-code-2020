import qualified Data.Text as T

day2 = do
    text <- readFile "day2/input"
    putStrLn $ "Part 1: " ++ (show $ solve isValidPartOne (parse text))
    putStrLn $ "Part 2: " ++ (show $ solve isValidPartTwo (parse text))

type Policy = (Char, Int, Int)
type InputLine = (Policy, String)

solve :: (InputLine -> Bool) -> [InputLine] -> Int
solve policy input = length $ filter (True==) (map policy input)

parse :: String -> [InputLine]
parse t = map parseInputLine $ lines t

parseInputLine :: String -> InputLine
parseInputLine input = (parsePolicy c', p')
    where
        c:p:_ = T.splitOn (T.pack ":") (T.pack input)
        c' = T.unpack $ T.strip c
        p' = T.unpack $ T.strip p

parsePolicy :: String -> Policy
parsePolicy s = (char, min, max)
    where
        minT:remT:_   = T.splitOn (T.pack "-") (T.pack s)
        min           = read $ T.unpack minT
        maxT:charT:_  = T.splitOn (T.pack " ") remT
        max           = read $ T.unpack maxT
        char          = head $ T.unpack $ T.strip charT

isValidPartOne :: InputLine -> Bool
isValidPartOne ((c, min, max), input) = matches >= min && matches <= max
    where matches = length $ filter (c==) input

isValidPartTwo :: InputLine -> Bool
isValidPartTwo ((c, a, b), input) = (input !! (a - 1) == c) /= (input !! (b - 1) == c)