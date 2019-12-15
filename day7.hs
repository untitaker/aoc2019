import Data.List
-- defining operators to be like in Python 3
(%) :: Int -> Int -> Int
(%) = mod
(//) :: Int -> Int -> Int
(//) = div

-- get nth digit from x counting from right (starting at 1)
(!) :: Int -> Int -> Int
x ! n = x % (10 ^ n) // (10 ^ (n-1))

type Output = [Int]
type Input = [Int]
type RawTape = [Int]
type Tape = (Int -> Int)
type TapeLen = Int

tapeFromRaw :: RawTape -> Tape
tapeFromRaw raw = (!!) raw

tapeToRaw :: Tape -> TapeLen -> RawTape
tapeToRaw tape len = map tape [0..(len - 1)]

getArgument :: Tape -> Int -> Int -> Int
getArgument tape i n = getWithMode mode
    where
    mode = (tape i) ! (n + 2)
    getWithMode 0 = tape $ tape $ i + n  -- position mode
    getWithMode 1 = tape $ i + n         -- immediate mode
    getWithMode m = error ("unknown mode " ++ show m)

run :: Int -> Tape -> Input -> Output
run i tape input | op == 99 = []
                 | otherwise = newNewOutput op
    where
    op = (tape i) % 100
    argPos n = tape $ i + n 
    arg = getArgument tape i

    -- defining these values here gives us infinite speedup due to caching
    -- (compared to inlining the function call)
    argPos1 = argPos 1
    argPos3 = argPos 3
    arg1 = arg 1
    arg2 = arg 2

    -- tape state transition, functions that modify the tape
    newTape 1 x | x == argPos3 = arg1 + arg2
    newTape 2 x | x == argPos3 = arg1 * arg2
    newTape 3 x | x == argPos1 = head input
    newTape 7 x | x == argPos3 = if arg1 < arg2 then 1 else 0
    newTape 8 x | x == argPos3 = if arg1 == arg2 then 1 else 0
    newTape _ x = tape x

    -- input state transition, functions that read from input
    newInput 3 = tail input
    newInput _ = input

    -- instruction pointer transition.
    -- enumerate all known instructions here
    newI 1 = i + 4
    newI 2 = i + 4
    newI 3 = i + 2
    newI 4 = i + 2
    newI 5 | arg1 /= 0 = arg2
           | otherwise = i + 3
    newI 6 | arg1 == 0 = arg2
           | otherwise = i + 3
    newI 7 = i + 4
    newI 8 = i + 4
    newI op' = error ("unknown op " ++ show op')

    -- run next transitions
    newOutput = run (newI op) (newTape op) (newInput op)

    -- output state transition, functions that write to output
    newNewOutput 4 = (arg 1):newOutput
    newNewOutput _ = newOutput

runProgram :: RawTape -> Input -> Output
runProgram rawTape input = output
    where
    output = run 0 tape input
    tape = tapeFromRaw rawTape

type PhaseSetting = Int
type ThrusterSignal = Int

amplifierProgram = [3,8,1001,8,10,8,105,1,0,0,21,38,55,64,81,106,187,268,349,430,99999,3,9,101,2,9,9,1002,9,2,9,101,5,9,9,4,9,99,3,9,102,2,9,9,101,3,9,9,1002,9,4,9,4,9,99,3,9,102,2,9,9,4,9,99,3,9,1002,9,5,9,1001,9,4,9,102,4,9,9,4,9,99,3,9,102,2,9,9,1001,9,5,9,102,3,9,9,1001,9,4,9,102,5,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,99]

runAmplifiers :: RawTape -> [PhaseSetting] -> [ThrusterSignal]
runAmplifiers program ps = a (length ps - 1)
    where
    a 0 = runProgram program ((ps !! 0) : 0 : (a (length ps - 1)))
    a n = runProgram program ((ps !! n) : (a (n-1)))

allSettings = permutations [0..4]
loopSettings = permutations [5..9]

getMaxSignal1 program = maximum [head $ runAmplifiers program settings | settings <- allSettings]
getMaxSignal2 program = maximum [last $ runAmplifiers program settings | settings <- loopSettings]

solution1 = getMaxSignal1 amplifierProgram
solution2 = getMaxSignal2 amplifierProgram
