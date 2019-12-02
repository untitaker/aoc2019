
type RawTape = [Int]
type Tape = (Int -> Int)
type TapeLen = Int

tapeFromRaw :: RawTape -> Tape
tapeFromRaw raw = (!!) raw

tapeToRaw :: Tape -> TapeLen -> RawTape
tapeToRaw tape len = map tape [0..(len - 1)]

run :: Int -> Tape -> Tape
run i tape | op == 99 = tape
           | otherwise = newTape
    where
    newTape = run (i + 4) (runStep op)

    op = tape i
    arg1 = tape $ tape $ i + 1
    arg2 = tape $ tape $ i + 2
    outputPos = tape $ i + 3

    runStep 1 x | x == outputPos = arg1 + arg2
                | otherwise = tape x
    runStep 2 x | x == outputPos = arg1 * arg2
                | otherwise = tape x
    runStep op x = error ("unknown op " ++ (show op))

program :: [Int]
program = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,6,19,23,2,6,23,27,1,5,27,31,2,31,9,35,1,35,5,39,1,39,5,43,1,43,10,47,2,6,47,51,1,51,5,55,2,55,6,59,1,5,59,63,2,63,6,67,1,5,67,71,1,71,6,75,2,75,10,79,1,79,5,83,2,83,6,87,1,87,5,91,2,9,91,95,1,95,6,99,2,9,99,103,2,9,103,107,1,5,107,111,1,111,5,115,1,115,13,119,1,13,119,123,2,6,123,127,1,5,127,131,1,9,131,135,1,135,9,139,2,139,6,143,1,143,5,147,2,147,6,151,1,5,151,155,2,6,155,159,1,159,2,163,1,9,163,0,99,2,0,14,0]

runProgram noun verb = head $ tapeToRaw (run 0 tape) tapeLen
    where
    tapeLen = length program
    tape 1 = noun
    tape 2 = verb
    tape x = tapeFromRaw program x

findInput output = head [[noun, verb] | noun <- [0..99], verb <- [0..99], runProgram noun verb == output]

solution1 = runProgram 12 2

solution2 = 100 * noun + verb
    where
    [noun, verb] = findInput 19690720
