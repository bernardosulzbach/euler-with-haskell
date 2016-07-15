import System.IO

-- This solution collapses the triangle bottom up.
--
-- Example:
--
--    1         1
--   2 3   ->  7 9  ->  10
--  4 5 6
--
-- This is very fast as it is O(n) in respect to element count.

inputFileName = "project-euler-018-input.txt"

stringToInteger string = toInteger (read string :: Int)

toIntegerList strings = map stringToInteger strings

readTriangle string = map toIntegerList (map words (lines string))

pairs :: [Integer] -> [(Integer, Integer)]
pairs list = zip list (tail list)

collapseDecision :: (Integer, (Integer, Integer)) -> Integer
collapseDecision decision = (fst decision) + maximum [(fst (snd decision)), (snd (snd decision))]

collapseLine :: Int -> [[Integer]] -> [Integer]
collapseLine line triangle
    | line == pred (length triangle) = triangle !! line
    | otherwise = map collapseDecision (zip (triangle !! line) (pairs (collapseLine (succ line) triangle)))

collapse triangle = collapseLine 0 triangle

maximumPathSum triangle = head (collapse triangle)

main = do
    input <- readFile inputFileName
    let triangle = readTriangle input
    print (maximumPathSum triangle)
