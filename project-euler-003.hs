import System.IO

-- Evaluates whether or not n is not divisible by d
doesNotDivide n d = mod n d /= 0

isPrime n = all (\ d -> doesNotDivide n d) [2..(truncate (sqrt (fromIntegral n)))]

-- Infinite list of primes
primes = filter isPrime [2..]

divideBySmallestPrimeFactor n = quot n (head (filter (\ x -> mod n x == 0) primes))

largestPrimeFactor n
    | isPrime n = n
    | otherwise = largestPrimeFactor (divideBySmallestPrimeFactor n)

inputNumber = 600851475143

main = do print (largestPrimeFactor inputNumber)
