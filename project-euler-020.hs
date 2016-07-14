import System.IO

sumOfDigits n
    | n == 0 = 0
    | otherwise = (mod n 10) + (sumOfDigits (quot n 10))

factorial n = product [1..n]

main = do print (sumOfDigits (factorial 100))
