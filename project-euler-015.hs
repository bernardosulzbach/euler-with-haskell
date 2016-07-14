import System.IO

-- Normally we could build a matrix of the form
-- 0   1   1   1   1
-- 1   2   3   4   5 ...
-- 1   3   6  10  15
--        ...
--
-- Where a(i, j) = a(i - 1, j) + a(i, j - 1)
--
-- Here, however, we will build a list of 19 integers of the form [1 ... 1]
-- Then derive the subsequent lines from it until we get the line we need.

firstLine = replicate 20 1

nextLineStep line value
    | null line = line
    | otherwise = (head line) + value : nextLineStep (tail line) ((head line) + value)

nextLine line = nextLineStep line 1  -- The first value of each row would be 1 

-- iterate produces an infinite list of the form [a, f(a), f(f(a)), ...]
-- Then we get the 20th element, which corresponds to the 20th row.
pathCount = last ((iterate nextLine firstLine) !! 20)
 
main = do print pathCount
