
import qualified Data.Text as T
import Data.List (nubBy)

-- Kill me
--        ,
--        \`-._           __
--         \\  `-..____,.'  `.
--          :`.         /    \`.
--          :  )       :      : \
--           ;'        '   ;  |  :
--           )..      .. .:.`.;  :
--          /::...  .:::...   ` ;
--          ; _ '    __        /:\
--          `:o>   /\o_>      ;:. `.
--         `-`.__ ;   __..--- /:.   \
--         === \_/   ;=====_.':.     ;
--          ,/'`--'...`--....        ;
--               ;                    ;
--             .'                      ;
--           .'                        ;
--         .'     ..     ,      .       ;
--        :       ::..  /      ;::.     |
--       /      `.;::.  |       ;:..    ;
--      :         |:.   :       ;:.    ;
--      :         ::     ;:..   |.    ;
--       :       :;      :::....|     |
--       /\     ,/ \      ;:::::;     ;
--     .:. \:..|    :     ; '.--|     ;
--    ::.  :''  `-.,,;     ;'   ;     ;
-- .-'. _.'\      / `;      \,__:      \
-- `---'    `----'   ;      /    \,.,,,/

day7 = do
    text <- readFile "day7/input"
    let rules = map parseRules $ lines text
        part1 = growRecursive (growUp rules) [initialValue]
        part2 = solvePartTwo rules [initialValue]
        in putStrLn $ "Part 1: " ++ (show $ length part1) ++ "\nPart 2: " ++ (show part2)

type BagSpec = (String, [(Int, String)])

initialValue = parseRules "shiny gold bags contain 3 bright turquoise bags, 1 striped purple bag, 5 mirrored white bags, 1 bright teal bag."
-- initialValue = parseRules "shiny gold bags contain 2 dark red bags."
-- initialValue = parseRules "dark green bags contain 2 dark blue bags."

growUp :: [BagSpec] -> [BagSpec] -> [BagSpec]
growUp all names  = filter (\x -> any ($ x) predicates) all
    where
        predicates = map canContainBag (map fst names)

-- filter (any (== "shiny gold")) <$>  map (\y -> map snd (snd y)) <$> map parseRules <$> lines <$> readFile "day7/input" >>= (mapM_ print)
-- (concat $ map (\y -> map snd (snd y)) names)

growRecursive :: ([BagSpec] -> [BagSpec]) -> [BagSpec] -> [BagSpec]
growRecursive f xs
    | xs == ys = xs
    | otherwise = growRecursive f ys
    where
        ys = nubBy (\x  y -> (fst x) == (fst y)) $ xs ++ (f xs)

solvePartTwo :: [BagSpec] -> [BagSpec] -> Int
solvePartTwo allSpecs initialValue = 1 + (sum $ concat $ map (\(_, subRules) -> map (\(c, n) -> c * (sptByName n)) subRules) initialValue)
    where
        sptByName = \name -> solvePartTwo allSpecs [(getRuleByName allSpecs name)]
        

getRuleByName :: [BagSpec] -> String -> BagSpec
getRuleByName allSpecs searchName = head $ filter (\x -> (fst x) == searchName) allSpecs

getAllBagsContainedIn :: [BagSpec] -> String -> [BagSpec]
getAllBagsContainedIn allSpecs searchName = filter (\x -> (fst x) `elem` searchNames) allSpecs
    where
        searchNames = map snd (snd searchRecord)
        searchRecord = head $ filter (\s -> fst s == searchName) allSpecs

-- Returns true if a bag with `bagName` is allowed to be inside of the BagSpec
canContainBag :: String -> BagSpec -> Bool
canContainBag bagName (_,c) = any ((bagName==) . snd) c



parseRules :: String -> BagSpec
parseRules s = (T.unpack a, split3)
    where
        split1 = T.splitOn (T.pack "bags contain") (T.pack s)
        (a,b) = (T.strip $ split1 !! 0, T.strip $ split1 !! 1)
        split2 = T.splitOn (T.pack ", ") b >>= sanitizeB
        split3 = map parseRule split2

-- Remove String `a` from a T.Text   
strip a = T.replace (T.pack a) (T.pack "")

parseRule :: T.Text -> (Int, String)
parseRule x = (read (T.unpack a) :: Int, T.unpack $ T.strip b)
    where
        (a, b) = T.splitAt 1 x

sanitizeB :: T.Text -> [T.Text]
sanitizeB b
    | b == (T.pack "no other bags.") = []
    | otherwise = (:[]) . (strip "bag") . (strip "bags") . (strip ".") $ b