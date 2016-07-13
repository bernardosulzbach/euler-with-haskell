import Data.List
import System.IO

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Counts the digits of a non-negative integer
count_digits n
    | n < 10 = 1
    | otherwise = 1 + count_digits (quot n 10)

main = do print (elemIndex (head (filter (\ x -> (count_digits x) >= 1000) fibs)) fibs)
