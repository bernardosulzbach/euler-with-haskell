import Data.Ratio

-- This solution is fast enough to solve HackerRank's version of the problem,
-- in which we need to find up to the 30,000th term.

continuedFractionTerm :: Int -> Int
continuedFractionTerm n
    | n == 0 = 2
    | rem (n - 1) 3 == 0 || rem (n - 3) 3 == 0 = 1
    | otherwise = 2 * ((quot (n - 2) 3) + 1)

evaluateContinuedFractionStep :: (Int -> Int) -> Int -> Int -> Rational
evaluateContinuedFractionStep rule step maxStep
    | step >= maxStep = 0
    | otherwise = (1 :: Rational) / (current + remaining)
    where
        current = fromIntegral (rule step)
        remaining = evaluateContinuedFractionStep rule (step + 1) maxStep

evaluateContinuedFraction :: (Int -> Int) -> Int -> Rational
evaluateContinuedFraction rule steps = initialValue + continuedFraction
    where
        initialValue = fromIntegral (rule 0)
        continuedFraction = evaluateContinuedFractionStep rule 1 steps

digitSum number
    | number < 10 = number
    | otherwise = (mod number 10) + digitSum (quot number 10)

main = do print (digitSum value)
    where
        value = numerator (evaluateContinuedFraction continuedFractionTerm 100)
