import qualified Data.Text as T
import Data.List (nub, intersect)

day6 = do
    text <- readFile "day6/input"
    print $ sum $ map (length . nub . filter (/='\n')) $ splitGroups text
    print $ sum $ map (length . foldl1 intersect . lines) $ splitGroups text

splitGroups :: String -> [String]
splitGroups input = map (T.unpack) $ T.splitOn (T.pack "\n\n") (T.pack input)
