import System.IO

-- We need
--   start - 1 + 1 * (skip + 1)
-- + start - 1 + 2 * (skip + 1)
-- + start - 1 + 3 * (skip + 1)
-- + start - 1 + 4 * (skip + 1)
-- = 4 (start - 1) + 10 * (skip + 1)
sumSpiralDiagonals start skip = 4 * (start - 1) + 10 * (skip + 1)

evaluateSpiral side = sumSpiralDiagonals ((side - 2) ^ 2 + 1) (side - 2)

sumDiagonals side
    | side == 1 = 1
    | otherwise = (evaluateSpiral side) + (sumDiagonals (side - 2))

main = do print (sumDiagonals 1001)
