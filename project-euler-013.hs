import System.IO

inputFileName = "project-euler-013-input.txt"

stringToInteger string = toInteger (read string :: Integer)

toIntegerList strings = map stringToInteger strings

readIntegers string = toIntegerList (lines string)

main = do
    input <- readFile inputFileName
    let integers = readIntegers input
    putStrLn (take 10 (show (sum integers)))
