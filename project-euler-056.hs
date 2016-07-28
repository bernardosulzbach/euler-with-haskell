maxValue = 100
maxValueAsInteger = fromIntegral maxValue

digitSum :: Integer -> Int
digitSum n
    | n < 10 = (fromIntegral n)
    | otherwise = digitSum (quot n 10) + (fromIntegral (mod n 10))

multiply a b = a * b

-- Returns the maximum digit sum of the first powers of the base.
maximumDigitSumInPowers base powers
    = maximum (take powers (map digitSum (iterate (multiply base) base)))

maximumDigitSum bases powers
    = maximum (map (\ base -> maximumDigitSumInPowers base powers) bases)

main = do print (maximumDigitSum [1 .. maxValueAsInteger] maxValue)
