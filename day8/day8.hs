
import Data.List (isPrefixOf, stripPrefix)
import qualified Data.Text as T

day8 = do
    program <- map parseInstruction <$> lines <$> readFile "day8/program.int"
    print $ solvePartOne program
    print $ solvePartTwo program


solvePartOne :: Program -> Int
solvePartOne p = doExecuteWithDuplicateCheck p [] 0

solvePartTwo :: Program -> Int
solvePartTwo p = execute fixedProgram
    where
        fixedProgram = head $ filter isValidProgram $ getAllMutations p

-- Execute programs

execute :: Program -> Int
execute p = doExecute p 0

doExecute :: Program -> Int -> Int
doExecute program pointer
    | pointer >= length program = 0
    | otherwise = case (program !! pointer) of
        Nop _   -> doExecute program (pointer + 1)
        Jmp arg -> doExecute program (pointer + arg)
        Acc arg -> arg + (doExecute program (pointer + 1))

doExecuteWithDuplicateCheck :: Program -> [Int] -> Int -> Int
doExecuteWithDuplicateCheck program executedInstructions pointer
    | pointer >= length program = 0
    | pointer `elem` executedInstructions = 0 -- Stop on loop detection
    | otherwise = case (program !! pointer) of
        Nop _   -> self (pointer + 1)
        Jmp arg -> self (pointer + arg)
        Acc arg -> arg + (self (pointer + 1))
    where
        self = doExecuteWithDuplicateCheck program (pointer:executedInstructions) 

isValidProgram p = isValidProgram' p [] 0 0

isValidProgram' :: Program -> [Int] -> Int -> Int -> Bool
isValidProgram' program executedInstructions pointer acc
    | pointer >= length program = True
    | pointer `elem` executedInstructions = False
    | otherwise = case (program !! pointer) of
        Nop _   -> self (pointer + 1) acc
        Jmp arg -> self (pointer + arg) acc
        Acc arg -> self (pointer + 1) (acc + arg)
    where
        self = isValidProgram' program (pointer:executedInstructions) 

-- Program mutation

getAllMutations :: Program -> [Program]
getAllMutations p = foldl (\acc ii -> acc ++ [mutateProgramAtIndex p ii] ) [] [0..programLength]
    where
        programLength = length p

mutateProgramAtIndex :: Program -> Int -> Program
mutateProgramAtIndex p i = (take i p) ++ [(mutate (p !! i))] ++ (drop (i+1) p)

mutate :: Instruction -> Instruction
mutate (Jmp x) = Nop x
mutate (Nop x) = Jmp x
mutate x = x

-- Instruction parsing
type Program = [Instruction]
data Instruction = Nop Int | Acc Int | Jmp Int
    deriving Show

parseInstruction :: String -> Instruction
parseInstruction xs
    | "nop " `isPrefixOf` xs = Nop arg
    | "acc " `isPrefixOf` xs = Acc arg
    | "jmp " `isPrefixOf` xs = Jmp arg
    where 
        arg = customToInt $ T.unpack $ T.splitOn (T.pack " ") (T.pack xs) !! 1

customToInt :: String -> Int
customToInt ('+':xs) = read xs
customToInt ('-':xs) = -1 * read xs
