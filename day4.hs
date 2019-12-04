import Data.List

-- defining operators to be like in Python 3
(%) = mod
(//) = div

-- get nth digit from x counting from right (starting at 1)
x ! n = x % (10 ^ n) // (10 ^ (n-1))

-- get all but leftmost digits of integer
digitTail x n = x - ((x ! n) * 10 ^ (n - 1))

-- repeat digit x n times
repeatDigit :: Int -> Int -> Int
repeatDigit x 1 = x
repeatDigit x n = repeatDigit x (n-1) * 10 + x

-- generates n-digit numbers whose digits are ascending in order, but fast
-- does not create unnecessarily large ranges that have to be filtered
generateDigits :: Int -> Int -> [Int]
generateDigits 1 start = [start..9]
generateDigits n start = sol
    where
    minX = start ! n
    sol = [ x * (10 ^ (n-1)) + y | x <- [minX..9], y <- generateDigits (n-1) (getNewStart n start x) ]

-- calculate new start position for recursive call of generateDigits
getNewStart n start x | x == minX = max (digitTail start n) digitBounded
                      | otherwise = digitBounded
    where
    minX = start ! n
    digitBounded = repeatDigit x (n-1)

-- implement upper side of range... since everything is lazily computed we can
-- just stream the numbers and stop when we need to
getRange digits start end = takeWhile (<= end) $ generateDigits digits start

gotDubs 0 = False
gotDubs x = found || next
    where
    found = (x ! 1) == (x ! 2)
    next = gotDubs (x // 10)

gotRealDubsImpl 0 prevDigit = False
gotRealDubsImpl x prevDigit = found && hasBorders || next
    where
    next = gotRealDubsImpl (x // 10) (x ! 1)
    found = (x ! 1) == (x ! 2) && hasBorders
    hasBorders = prevDigit /= (x ! 1) && (x ! 2) /= (x ! 3)

gotRealDubs x = gotRealDubsImpl x (-1)

ranges = getRange 6 240920 789857
solution1 = length $ filter gotDubs $ ranges
solution2 = length $ filter gotRealDubs $ ranges
