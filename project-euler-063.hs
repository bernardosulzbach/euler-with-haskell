import System.IO

-- We have to find x such that
--
--   10 ^ (n - 1) <= x ^ n < 10 ^ n
--
--   n - 1 <= n log10(x) < n
--
--   (n - 1) / n <= log10(x) < 1
--
--   10 ^ ((n - 1) / n) <= x < 10
--
-- Therefore we count how many integer values of n satisfy the condition
--
--   10 ^ ((n - 1) / n) <= x
--
-- For all x in [1..9]

checkExponentAboveLimit :: Integer -> Integer -> Bool
checkExponentAboveLimit x n = 10 ** ((fromIntegral (n - 1)) / (fromIntegral n)) > (fromIntegral x)

buildSolutions :: Integer -> Integer -> [Integer] -> [Integer]
buildSolutions x n solutions
    | checkExponentAboveLimit x n = solutions
    | otherwise = buildSolutions x (n + 1) ((x ^ n) : solutions)

solutionCount x = length (buildSolutions x 1 [])

main = do print (sum (map solutionCount [1..9]))
