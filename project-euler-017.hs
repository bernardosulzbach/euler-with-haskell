import           Data.Char
import           System.IO


prefixIfNotZero :: String -> String -> String
prefixIfNotZero prefix "zero" = ""
prefixIfNotZero prefix bit    = prefix ++ bit

toString :: (Integral a) => a -> String
toString 0 = "zero"
toString 1 = "one"
toString 2 = "two"
toString 3 = "three"
toString 4 = "four"
toString 5 = "five"
toString 6 = "six"
toString 7 = "seven"
toString 8 = "eight"
toString 9 = "nine"
toString 10 = "ten"
toString 11 = "eleven"
toString 12 = "twelve"
toString 13 = "thirteen"
toString 14 = "fourteen"
toString 15 = "fifteen"
toString 16 = "sixteen"
toString 17 = "seventeen"
toString 18 = "eighteen"
toString 19 = "nineteen"
toString 20 = "twenty"
toString 30 = "thirty"
toString 40 = "forty"
toString 50 = "fifty"
toString 60 = "sixty"
toString 70 = "seventy"
toString 80 = "eighty"
toString 90 = "ninety"
toString x
  | x < 100 = (toString (x - mod x 10)) ++ (prefixIfNotZero "-" (toString (mod x 10)))
  | x < 1000 = (toString (div (x - mod x 100) 100)) ++ " hundred" ++ (prefixIfNotZero " and " (toString (mod x 100)))
  | otherwise = (toString (div (x - mod x 1000) 1000)) ++ " thousand" ++ (prefixIfNotZero " and " (toString (mod x 1000)))


countLetters :: String -> Integer
countLetters [] = 0
countLetters (h:t)
  |   isAlpha h = countLetters t + 1
  |   otherwise = countLetters t

main = do print $ sum (map countLetters (map toString [1..1000]))
