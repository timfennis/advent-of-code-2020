import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (intersect, isSuffixOf, isPrefixOf)
import Data.Set (Set)
import Data.Char (isAlpha)

day4 = do 
    text <- readFile "day4/input"
    -- mapM_ print $ parsePassports text
    print $ solve1 $ parsePassports text
    print $ solve2 $ parsePassports text

type Field = (String, String)
type Passport = M.Map String String

solve1 :: [Passport] -> Int
solve1 passports = length $ filter hasRequiredFields passports

solve2 :: [Passport] -> Int
solve2 passports = length $ filter ((&&) <$> hasRequiredFields <*> isValid) passports

requiredFields = S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
validEyeColors = S.fromList ["amb","blu","brn","gry","grn","hzl","oth"]

-- Returns True if the input string only contains numerical characters
isNumeric :: String -> Bool
isNumeric = all (\c -> elem c ['0','1','2','3','4','5','6','7','8','9'])

-- Returns True if the input string only contains hexadecimal (lower case) characters
isHex :: String -> Bool
isHex = all (\c -> elem c ['a','b','c','d','e','f','0','1','2','3','4','5','6','7','8','9'])

-- Check if a passport map has all the required keys
hasRequiredFields :: Passport -> Bool
hasRequiredFields passport = requiredFields `S.isSubsetOf` (M.keysSet passport)

-- Checks if a passport's fields match all the rules given in part 2 of the problem
isValid :: Passport -> Bool
isValid passport = (byr >= 1920 && byr <= 2002) 
            && (iyr >= 2010 && iyr <= 2020) 
            && (eyr >= 2020 && eyr <= 2030)
            && (("in" `isSuffixOf` hgt && hgtNum >= 59 && hgtNum <= 76) || ("cm" `isSuffixOf` hgt && hgtNum >= 150 && hgtNum <= 193))
            && (head hcl == '#') 
            && (isHex $ tail hcl)
            && (S.member ecl validEyeColors)
            && (length pid == 9 && isNumeric pid) 
    where
        byr = read $ passport M.! "byr" :: Int
        iyr = read $ passport M.! "iyr" :: Int
        eyr = read $ passport M.! "eyr" :: Int
        hgt = passport M.! "hgt"
        hgtNum = (read $ filter (not . isAlpha) hgt :: Int)
        hcl = passport M.! "hcl"
        ecl = passport M.! "ecl"
        pid = passport M.! "pid"

-- Takes a string describing multiple passports and converts it to a list of passports.
-- Two '\n' characters are used to separate different passports
parsePassports :: String -> [Passport]
parsePassports text = map parsePassport $ T.splitOn (T.pack "\n\n") (T.pack text)

-- "Takes a String formatted accoring to the rules described in the puzzle and converts it to a Map of Strings to String"
-- Example: parsePassport (T.pack "foo:bar\nbaz:baz")
parsePassport :: T.Text -> Passport
parsePassport text = M.fromList $ map parseField fields
    where
        fields = T.splitOn (T.pack " ") text >>= (T.splitOn (T.pack "\n"))

-- Takes a T.Text formated like this "foo:bar" and converts it to a tuple ("foo", "bar")
parseField :: T.Text -> Field
parseField text = (T.unpack (head x), T.unpack (x !! 1))
    where
        x = T.splitOn (T.pack ":") text
