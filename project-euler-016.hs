import System.IO

sumOfDigits n
    | n == 0 = 0
    | otherwise = (mod n 10) + (sumOfDigits (quot n 10))

main = do print (sumOfDigits (2 ^ 1000))
