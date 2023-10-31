data Alphabet = Zero | One
    deriving (Show, Eq)

type Word = [Alphabet]

printer :: Main.Word -> String
printer [] = ""
printer (Zero:xs) = "0" ++ printer xs
printer (One:xs) = "1" ++ printer xs

-- Encapsulates the state of the system at a snapshot
data State = State [Main.Word] Main.Word Int
instance Show State where
    show (State p w i) = printer w ++ " at step " ++ show i

-- Variables to set to run the machine
-- Production rules, set to cycle infinitely
productions :: [Main.Word]
productions = cycle [[Zero, One, One],
                    [One, Zero, One, Zero],
                    [One, One, One]]

-- Starting word
word :: Main.Word
word = [One, Zero, One, Zero]

-- Number of steps to run the machine for
steps :: Int
steps = 15

-- Logic to update the machine at each step

-- Starting state
state :: State
state = State productions word steps

update :: State -> State
update (State p w 0) = State p w 0
update (State (p:productions) (Zero:xs) i) = State productions xs (i - 1)
update (State (p:productions) (One:xs) i) = State productions (xs ++ p) (i - 1)

-- Running the actual machine and printing the output

machine :: [State]
machine = iterate update state

machineout :: [State]
machineout = take (steps + 1) machine

alignout :: State -> String
alignout (State p w i) = replicate (steps - i) ' ' ++ show (State p w i) ++ "\n"

printoutputs :: String
printoutputs = concatMap alignout machineout

-- Execute code
main :: IO ()
main = do
    putStr printoutputs