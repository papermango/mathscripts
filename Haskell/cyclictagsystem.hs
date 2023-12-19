import Options.Applicative
import Text.Read (readMaybe)
import Data.List.Split (splitOn)
import Data.List (dropWhileEnd)
import Data.Char (isSpace)

--- Basic type definitions ---

data Alphabet = Zero | One
    deriving (Show, Eq)

type Word = [Alphabet]

-- Helper function to print a word out, right-associative
printer :: Main.Word -> String
printer [] = ""
printer (Zero:xs) = "0" ++ printer xs
printer (One:xs) = "1" ++ printer xs

-- Encapsulates the state of the system at a snapshot
-- First Int is step count, second is number of productions (and should be kept constant)
data State = State [Main.Word] Main.Word Int Int
            | Halted Int
            | Empty

-- Defines Show and Eq on states
-- Eq is a weaker equality which considers repeated states equal
-- Possible way to find true repeats that accounts for duplicate words:
    -- zip productions with indices and match on both of them
instance Show State where
    show (State p w i _) = printer w ++ " at step " ++ show i ++ ", next production " ++ printer (head p)
    show (Halted i) = "Halted at step " ++ show i
    show Empty = "N/A"
instance Eq State where
    (==) (State p1 w1 i1 ps1) (State p2 w2 i2 ps2) = (head p1 == head p2) 
        && (w1 == w2) 
        && (ps1 == ps2) -- technically always true, since program execution only ever considers one CTS
        && (rem i1 ps1 == rem i2 ps2)
    (==) State {} _ = False
    (==) _ State {}  = False
    (==) _ _ = True

--- Running the machine ---

-- Logic to update the machine at each step
update :: State -> State
update (State (p:productions) (Zero:xs) i ps) = State productions xs (i + 1) ps
update (State (p:productions) (One:xs) i ps) = State productions (xs ++ p) (i + 1) ps
update (State _ [] i _) = Halted i
update (State [] xs i _) = Empty -- invalid state; cycle throws error on an empty list, so this may not be needed
update (Halted i) = Empty
update Empty = Empty -- invalid state

-- Runs the machine off steps and an initial state, allows specifying productions and starting word instead with runMachine
runMachine :: Int -> State -> [State]
runMachine n initstate = take (n + 1) (iterate update initstate)

runMachineFromInputs :: Int -> [Main.Word] -> Main.Word -> [State]
runMachineFromInputs n productions word = runMachine n (State (cycle productions) word 0 (length productions))

-- Pretty-print each state in sequence
alignout :: State -> String
alignout s@(State p w i _) = replicate i ' ' ++ show s ++ "\n"
alignout s@(Halted i) = replicate (i + 1) ' ' ++ show s ++ "\n"
alignout Empty = ""
printoutputs :: [State] -> String
printoutputs = concatMap alignout

-- Searches for repeated states, and returns the first repeated state
-- Hangs forever if used on an infinite nonhalting CTS
findrepeatedstate :: [State] -> State
findrepeatedstate [] = Empty
findrepeatedstate (s:states) = if s `elem` states then s else findrepeatedstate states

-- Finds last state in a CTS
findLast :: [State] -> State
findLast [] = Empty
findLast (x:xs)
            | x == Halted 0 = x
            | null xs       = x
            | otherwise = findLast xs 
--- Default variables ---

defProds :: [Main.Word]
defProds = [[Zero, One, Zero],
                [One, Zero],
                [One, One],
                []]

-- Starting word
defWord :: Main.Word
defWord = [One, Zero, One, Zero, One]

--- Command line parser code, using optparse-applicative package ---

-- Available options
data Options = Options
    { verbose       :: Bool
    , stepsran      :: Int
    , detectrepeats :: Bool
    , inputIntMode  :: Bool
    , inputData     :: Maybe Input}

parseoptions :: Parser Options
parseoptions = Options
    <$> switch
        ( long "verbose"
        <> short 'v'
        <> help "Print the full machine runtime; otherwise, output end state only" )
    <*> option auto
        ( long "steps"
        <> short 'n'
        <> help "Steps count to run machine for"
        <> showDefault
        <> value 100
        <> metavar "INT" )
    <*> switch
        ( long "detectrepeats"
        <> short 'r'
        <> help "Look for a repeated state after running the machine")
    <*> switch
        ( long "integerinput"
        <> short 'i'
        <> help "Input initial machine state with unsigned integer representation" )
    <*> input -- Machine input parser, see below

-- Wraps parser up with high-level help text and adds --help
opts :: ParserInfo Options
opts = info (parseoptions <**> helper)
    ( fullDesc
    <> progDesc "Simulate a cyclic tag system to a given number of steps. Halts are recognized only if the register clears. Optionally looks for repeating states after execution, which is the standard way to interpret halting in a cyclic tag system. Looking for repeats is quadratic in the number of steps run and linear in register length."
    <> header "cyts - a cyclic tag system simulator written in Haskell")

-- Parser for machine input --

data Input
  = FileInput FilePath
  | StdInput
  | CompactInput String

fileInput :: Parser Input
fileInput = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Read initial machine state from input file FILENAME" )

stdInput :: Parser Input
stdInput = flag' StdInput
  (  long "stdin"
  <> short 's'
  <> help "Read initial machine state from stdin" )

cmdInput :: Parser Input
cmdInput = CompactInput <$> strOption
  (  long "compact"
  <> short 'c'
  <> help "Compact (scripting) mode. Reads initial machine state from command line as a comma-separated string and returns the step at which the machine halted, -1 if not halted. Overrides -v and -r." )

-- optional sets input to be optional
input :: Parser (Maybe Input)
input = optional $ fileInput <|> stdInput <|> cmdInput

--- Read machine input ---

-- Converts binary string into a Main.Word. Ignores symbols that are not 0 or 1
readBinStr :: String -> Main.Word
readBinStr "" = []
readBinStr (x:xs)
    | x == '0'  = Zero : readBinStr xs
    | x == '1'  = One : readBinStr xs
    | x == '-'  = []
    | otherwise = readBinStr xs
-- Reads off space-separated ints into a list of words
readBinStrs :: String -> [Main.Word]
readBinStrs = map readBinStr . words

-- Converts integer into word
readIntAsBin :: Int -> Main.Word
readIntAsBin x
    | x <= 1   = []
    | rem == 0 = readIntAsBin div ++ [Zero]
    | rem == 1 = readIntAsBin div ++ [One]
    | otherwise = []
    where (div, rem) = divMod x 2
-- Reads off space-separated ints into a list of words
readIntsAsBins :: String -> [Main.Word]
readIntsAsBins = map (readIntAsBin . maybe 0 id)
                    . filter (Nothing /=)
                    . map (\x -> readMaybe x :: Maybe Int)
                    . words

-- Trim whitespace, from StackOverflow
trim = dropWhileEnd isSpace . dropWhile isSpace

-- Reads off comma-separated ints into a list of words
readCIntsAsBins :: String -> [Main.Word]
-- readCIntsAsBins = map () . splitOn "," 

--- Execution code ---

main :: IO ()
main = execute =<< execParser opts

-- Main function to handle inputs
execute :: Options -> IO ()
-- No input; default execution
execute opts@(Options _ _ _ _ Nothing) = executeOnInput opts defProds defWord
-- Compact (scripting) input
-- Overrides two bucketed options, so it goes first
execute (Options _ steps _ True (Just (CompactInput input))) = print "Unimplemented"
execute (Options _ steps _ False (Just (CompactInput input))) =  print "Unimplemented"

-- File input
execute opts@(Options _ _ _ True (Just (FileInput fp))) = print "Unimplemented"
execute opts@(Options _ _ _ False (Just (FileInput fp))) =  print "Unimplemented"

-- Stdin input
execute opts@(Options _ _ _ True (Just StdInput)) = do
    putStrLn "Enter starting word as an integer: "
    wordstr <- getLine
    putStrLn "Enter list of productions as integers, space-separated: "
    prodstr <- getLine
    putStrLn "---"
    executeOnInput opts (readIntsAsBins prodstr) (head $ readIntsAsBins wordstr)

execute opts@(Options _ _ _ False (Just StdInput)) = do
    putStrLn "Enter starting word in binary: "
    wordstr <- getLine
    putStrLn "Enter list of productions in binary, space-separated: "
    prodstr <- getLine
    putStrLn "---"
    executeOnInput opts (readBinStrs prodstr) (head $ readBinStrs wordstr)

--- Print IO actions to stitch together program cmd output from ---

printProductions :: [Main.Word] -> IO ()
printProductions prod = putStr "Productions: " >> print (map printer prod) >> putStrLn ""

-- Prints a found repeated state in a given machine
printFoundRepeatedState :: [State] -> IO ()
printFoundRepeatedState = (putStr "Repeated/halted state at " >>) . print . findrepeatedstate

-- Runs and prints the machine, including repeated state if asked for via True bool
printShortMachine :: Int -> [Main.Word] -> Main.Word -> Bool -> IO ()
printShortMachine steps productions word findrepeats = do
    printProductions productions
    print (head machineout)
    putStrLn ""
    print (findLast machineout)
    if findrepeats then printFoundRepeatedState machineout else putStr ""
        where machineout = runMachineFromInputs steps productions word

-- Runs and prints the machine verbosely, with same behavior as above
printLongMachine :: Int -> [Main.Word] -> Main.Word -> Bool -> IO ()
printLongMachine steps productions word findrepeats = do
    printProductions productions
    putStrLn "Output:"
    putStrLn (printoutputs machineout)
    if findrepeats then printFoundRepeatedState machineout else putStr ""
        where machineout = runMachineFromInputs steps productions word

-- Handles non-input half of execute function
executeOnInput :: Options -> [Main.Word] -> Main.Word -> IO ()
executeOnInput (Options True n fr _ _) productions word = printLongMachine n productions word fr
executeOnInput (Options False n fr _ _) productions word = printShortMachine n productions word fr