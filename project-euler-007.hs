import System.IO

-- Evaluates whether or not n is not divisible by d
doesNotDivide n d = mod n d /= 0

isPrime n = all (\ d -> doesNotDivide n d) [2..(truncate (sqrt (fromIntegral n)))]

-- Infinite list of primes
primes = filter isPrime [2..]

main = do print (primes !! 10000)  -- Note that this is zero-indexed
