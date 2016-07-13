import System.IO

collatz_step :: Integer -> Integer -> Integer

collatz_step cur ste 
    | cur == 1 = (ste + 1)
    | mod cur 2 == 0 = collatz_step (quot cur 2) (ste + 1)
    | otherwise = collatz_step (3 * cur + 1) (ste + 1)

collatz start = collatz_step start 0

biggest_collatz a b
    | collatz a > collatz b = a
    | otherwise = b

main = do print (foldl biggest_collatz 1 [2..1000000])
