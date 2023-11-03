data Alphabet = Zero | One
    deriving (Show, Eq)

type Word = [Alphabet]

printer :: Main.Word -> String
printer [] = ""
printer (Zero:xs) = "0" ++ printer xs
printer (One:xs) = "1" ++ printer xs

-- Encapsulates the state of the system at a snapshot
data State = State [Main.Word] Main.Word Int | Halted Int | Empty
instance Show State where
    show (State p w i) = printer w ++ " at step " ++ show i
    show (Halted i) = "Halted at step " ++ show i

-- Logic to update the machine at each step

update :: State -> State
update (State (p:productions) (Zero:xs) i) = State productions xs (i + 1)
update (State (p:productions) (One:xs) i) = State productions (xs ++ p) (i + 1)
update (State _ [] i) = Halted i
update (Halted i) = Empty
update Empty = Empty

-- Running the actual machine and printing the output

machine :: [State]
machine = iterate update initstate

machineout :: [State]
machineout = take (steps + 1) machine

alignout :: State -> String
alignout (State p w i) = replicate i ' ' ++ show (State p w i) ++ "\n"
alignout (Halted i) = replicate (i + 1) ' ' ++ show (Halted i)
alignout Empty = ""

printoutputs :: String
printoutputs = concatMap alignout machineout

-- Variables to set to run the machine
-- Production rules
productions :: [Main.Word]
productions = [[Zero, One, One],
                [One, Zero, One, Zero],
                [One, One, One, Zero, Zero, Zero],
                [One, Zero, Zero, Zero]]

-- Set production rules to cycle infinitely
prodcycle :: [Main.Word]
prodcycle = cycle productions

-- Starting word
word :: Main.Word
word = [Zero, One, Zero, Zero, Zero, One]

-- Number of steps to run the machine for
steps :: Int
steps = 50

-- Starting state
initstate :: State
initstate = State prodcycle word 0

-- Execute code
main :: IO ()
main = do
    putStrLn "Productions:"
    print (map printer productions)
    putStrLn "Output:"
    putStr printoutputs