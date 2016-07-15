import Data.List
import System.IO

-- This solution is a combination of solutions 3 and 76.
-- We find out in how many ways we can partition a number only using primes.

-- Evaluates whether or not n is not divisible by d
doesNotDivide n d = mod n d /= 0

isPrime n = all (\ d -> doesNotDivide n d) [2..(truncate (sqrt (fromIntegral n)))]

-- Infinite list of primes
primes = filter isPrime [2..]

addInPlace src dst tab = (take dst tab) ++ [(tab !! src) + (tab !! dst)] ++ (drop (succ dst) tab)

addUntilEnd src dst max tab
    | dst == max = tab
    | otherwise = addUntilEnd (succ src) (succ dst) max (addInPlace src dst tab)

distribute cur tab = (addUntilEnd 0 cur (length tab) tab)

-- Uses the prime at index primeIndex
tableStep primeIndex size tab
    | primes !! primeIndex > size = tab  -- Stop after the primes get too big
    | otherwise = tableStep (succ primeIndex) size (distribute (primes !! primeIndex) tab)

-- Creates the "raw" partition table
-- According to this table 0 has 1 prime partition and all primes have one extra partition.
createRawTable n = tableStep 0 (succ n) (1 : replicate n 0)

predIfPrime n
    | isPrime n = (pred n)
    | otherwise = n

buildPrimePartitionTable n = 0 : (tail (map predIfPrime (createRawTable n)))

-- Empirically determined to be sufficient
tableSize = 100

-- Print better than "Nothing" if not found and remove "Just" if found
printMaybe x = maybe (putStrLn "Not found") print x

main = do printMaybe (findIndex (> 5000) (buildPrimePartitionTable tableSize))
