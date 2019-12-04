import Data.List

-- defining operators to be like in Python 3
(%) = mod
(//) = div

-- get nth digit from x counting from right (starting at 1)
digitAt n x = x % (10 ^ n) // (10 ^ (n-1))

-- get all but leftmost digits of integer
digitTail x n = x - ((digitAt n x) * 10 ^ (n - 1))

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
    minX = digitAt n start
    sol = [
        x * (10 ^ (n-1)) + y |
        x <- [minX..9],
        y <- (
            generateDigits
            (n-1)
            (getNewStart n start x)
         )
     ]

-- calculate new start position for recursive call of generateDigits
getNewStart n start x | x == minX = max (digitTail start n) digitBounded
                      | otherwise = digitBounded
    where
    minX = digitAt n start
    digitBounded = repeatDigit x (n-1)

-- implement upper side of range... since everything is lazily computed we can
-- just stream the numbers and stop when we need to
getRange digits start end = takeWhile (<= end) $ generateDigits digits start

gotDubs 0 = False
gotDubs x = found || next
    where
    found = (digitAt 1 x) == (digitAt 2 x)
    next = gotDubs (x // 10)

gotRealDubsImpl 0 prevDigit = False
gotRealDubsImpl x prevDigit = found && hasBorders || next
    where
    next = gotRealDubsImpl (x // 10) (digitAt 1 x)
    found = (digitAt 1 x) == (digitAt 2 x) && hasBorders
    hasBorders = prevDigit /= (digitAt 1 x) && (digitAt 2 x) /= (digitAt 3 x)

gotRealDubs x = gotRealDubsImpl x (-1)

ranges = getRange 6 240920 789857
solution1 = length $ filter gotDubs $ ranges
solution2 = length $ filter gotRealDubs $ ranges
