main :: IO()

generateSequence :: Int -> [Int]
generateSequence x
    | x <= 1 = []
    | y == 0 = x : generateSequence (truncate (sqrt (fromIntegral (x))) :: Int)
    | otherwise = x : generateSequence (x * 2)
    where y = x `mod` 3

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort [x] = [x]
quickSort (x: xs) = quickSort ([y | y <- xs, y > x] ++ [x] ++ [y | y <- xs, y <= x])

longestConsecutiveDescending :: [Int] -> [Int]
longestConsecutiveDescending [] = []
longestConsecutiveDescending [x] = [x]
longestConsecutiveDescending (x:y:xs)
    | x - 1 == y = x : longestConsecutiveDescending (y:xs)
    | otherwise = longest (x : getDescending(y: xs)) (longestConsecutiveDescending(y: xs))
    where 
        getDescending :: [Int] -> [Int]
        getDescending [] = []
        getDescending [a] = [a]
        getDescending (a: b: bc)
            | a - 1 == b = x : getDescending (b: bc)
            | otherwise = [a]

        longest a b 
            | length a < length b = b
            | otherwise = a

main = do 
    print(generateSequence 18)
    print(longestConsecutiveDescending (quickSort [5, 4, 6, 7, 3]))

