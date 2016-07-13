import System.IO

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

firstFibs = takeWhile (<= 4000000) fibs

isOdd x = mod x 2 == 0

main = do print (sum [x | x <- firstFibs, isOdd x])
