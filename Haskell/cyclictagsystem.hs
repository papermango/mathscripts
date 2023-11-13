import Options.Applicative

data Alphabet = Zero | One
    deriving (Show, Eq)

type Word = [Alphabet]

-- Helper function to print a word out, right-associative
printer :: Main.Word -> String
printer [] = ""
printer (Zero:xs) = "0" ++ printer xs
printer (One:xs) = "1" ++ printer xs

-- Encapsulates the state of the system at a snapshot
data State = State [Main.Word] Main.Word Int | Halted Int | Empty

-- Defines Show and Eq  on states
-- Eq is a weaker equality which considers repeated states equal
instance Show State where
    show :: State -> String
    show (State p w i) = printer w ++ " at step " ++ show i ++ ", next production " ++ printer (head p)
    show (Halted i) = "Halted at step " ++ show i
    show Empty = "N/A"
instance Eq State where
    (==) :: State -> State -> Bool
    (==) (State p1 w1 _) (State p2 w2 _) = (head p1 == head p2) && (w1 == w2)
    (==) (State {}) (Halted _) = False
    (==) (Halted _) (State {})  = False
    (==) (Halted _) (Halted _) = True
    (==) Empty Empty = True
    (==) (Halted _) Empty = True
    (==) Empty (Halted _) = True

-- Logic to update the machine at each step
update :: State -> State
update (State (p:productions) (Zero:xs) i) = State productions xs (i + 1)
update (State (p:productions) (One:xs) i) = State productions (xs ++ p) (i + 1)
update (State _ [] i) = Halted i
update (State [] xs i) = Empty -- invalid state
update (Halted i) = Halted i
update Empty = Empty -- invalid state

-- Runs the machine off steps and an initial state, allows specifying productions and starting word instead with runMachine --
runMachine :: Int -> State -> [State]
runMachine n initstate = take (n + 1) (iterate update initstate)

runMachineFromInputs :: Int -> [Main.Word] -> Main.Word -> [State]
runMachineFromInputs n productions word = runMachine n (State (cycle productions) word 0)

-- Pretty-print each state in sequence
alignout :: State -> String
alignout (State p w i) = replicate i ' ' ++ show (State p w i) ++ "\n"
alignout (Halted i) = replicate (i + 1) ' ' ++ show (Halted i)
alignout Empty = ""
printoutputs :: [State] -> String
printoutputs = concatMap alignout

-- Searches for repeated states, and returns the first repeated state
-- Hangs forever if used on an infinite nonhalting CTS
findrepeatedstate :: [State] -> State
findrepeatedstate [] = Empty
findrepeatedstate (s:states) = if s `elem` states then s else findrepeatedstate states

-- Variables to set to run the machine. In the future, these will be enterable --
-- Production rules
productions :: [Main.Word]
productions = [[Zero, One, Zero],
                [One, Zero],
                [One, One],
                []]

-- Starting word
word :: Main.Word
word = [One, Zero, One, Zero, One]

-- Parser code, using optparse-applicative package --

-- Available options
data Options = Options
    { verbose       :: Bool
    , stepsran      :: Int
    , detectrepeats :: Bool}

parseoptions :: Parser Options
parseoptions = Options
    <$> switch
        ( long "verbose"
        <> short 'v'
        <> help "Print the full machine runtime; otherwise, output end state only" )
    <*> option auto
        ( long "steps"
        <> short 's'
        <> help "Steps count to run machine for"
        <> showDefault
        <> value 100
        <> metavar "INT" )
    <*> switch
        ( long "detectrepeats"
        <> short 'r'
        <> help "Look for a repeated state after running the machine")

-- Wraps parser up with high-level help text and adds --help
opts :: ParserInfo Options
opts = info (parseoptions <**> helper)
    ( fullDesc
    <> progDesc "Simulate a cyclic tag system to a given number of steps. Halts are recognized only if the register clears. Optionally looks for repeating states after execution, which is the standard way to interpret halting in a cyclic tag system. Looking for repeats is quadratic in the number of steps run and linear in register length."
    <> header "ctshs - a cyclic tag system simulator written in Haskell")

-- Execution code --
main :: IO ()
main = execute =<< execParser opts

execute :: Options -> IO ()
execute (Options True n False) = do
    putStrLn "Productions:"
    print (map printer productions)
    putStrLn "Output:"
    putStr (printoutputs (runMachineFromInputs n productions word))

execute (Options True n True) = do
    putStrLn "Productions:"
    print (map printer productions)
    putStrLn "Output:"
    putStrLn (printoutputs machineout)
    putStr "Repeated state at "
    print (findrepeatedstate machineout)
        where machineout = runMachineFromInputs n productions word

execute (Options False n False) = print (last (runMachineFromInputs n productions word))

execute (Options False n True) = print (last machineout) >> putStr "Repeated state at " >> print (findrepeatedstate machineout)
    where machineout = runMachineFromInputs n productions word
