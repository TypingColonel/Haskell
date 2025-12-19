numSign :: Int -> String
evenOderOdd :: Int -> String
absValue :: Int -> Int
marks :: Int -> Char
classifyNumbers :: Int -> String
-- consonantUndVowels :: Char -> String    

numSign x | x > 0 = "Positive"
          | x < 0 = "Negative"
          | otherwise = "Zero"

evenOderOdd x | x == 0 = "zero"
              | y == 0 = "Even"
              | otherwise = "Odd"
              where y = x `mod` 2

absValue x | y == "Positive" || y == "Zero" = x
           | otherwise = (-1) * x
           where
                y = numSign x

marks x | x < 35 = 'F'
        | x < 50 = 'D'
        | x < 70 = 'C'
        | x < 80 = 'B'
        | x < 90 = 'A'
        | otherwise = 'O'
 
classifyNumbers x | x < 10 = "Small"
                  | x < 100 = "Medium"
                  | otherwise = "Large"

consonantsUndVowls x | elem x "aeiou" = "Vowels"
                     | otherwise = "consonants"

leapYear x | y == 0 = "Leap Year"
           | otherwise = "Not a leap year"                      
           where    
                y = x `mod` 4 

palindrome xs | xs == reverse xs = True
              | otherwise = False

main = do
    print(numSign 12)
    print(evenOderOdd 12)
    print(absValue (-12))