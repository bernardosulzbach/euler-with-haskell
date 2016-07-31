import Data.List
import Data.Maybe

minimumDigits = 1000

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Counts the digits of a non-negative integer
countDigits n
    | n < 10 = 1
    | otherwise = 1 + countDigits (quot n 10)

counts = fmap countDigits fibs

main = do print (fromJust (findIndex (>= minimumDigits) counts))
