import System.IO

main = do print (abs ((sum [x ^ 2 | x <- [1..100]]) - ((sum [1..100]) ^ 2)))
