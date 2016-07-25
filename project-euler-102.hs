import System.IO

-- A triangle is represented by 6 integers.
-- We check if for all sides, the origin produces the same sign for
--   (x - x1)(y2 - y1) - (y - y1)(x2 - x1)

inputFileName = "project-euler-102-input.txt"

stringToInteger string = toInteger (read string :: Integer)

toIntegerList strings = map stringToInteger strings

splitStringIncremental str sym strs
    | null str = strs
    | head str == sym = splitStringIncrmeental
splitString str sym = splitStringIncremental str sym null

readTriangleList string = map toIntegerList (map words (lines string))

orientation x y x1 y1 x2 y1 = (x - x1) * (y2 - y1) - (y - y1) * (x2 - x1)

allEqual list = and (map (== head list) (tail list))

sameSign x y z = allEqual (map signum [x y z])

containsOrigin triangle = sameSign
    (orientation 0 0 (triangle !! 0) (triangle !! 1) (triangle !! 2) (triangle !! 3))
    (orientation 0 0 (triangle !! 2) (triangle !! 3) (triangle !! 4) (triangle !! 5))
    (orientation 0 0 (triangle !! 4) (triangle !! 5) (triangle !! 0) (triangle !! 1))

main = do
    input <- readFile inputFileName
    let triangleList = toTriangleList input
    print (length (filter (containsOrigin triangleList)))
