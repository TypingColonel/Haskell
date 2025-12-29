main :: IO()

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x: xs) = 
    quickSort [y | y <- xs, y <= x] 
    ++
    [x]
    ++
    quickSort [y | y <- xs, y > x]

longestConsecutive :: [Int] -> [Int]
longestConsecutive [] = []
longestConsecutive [x] = [x]
longestConsecutive xs = helper (quickSort xs)
    where 
        helper :: [Int] -> [Int]
        helper [] = []
        helper [x] = [x]
        helper (x: y: xs) 
            | y == x + 1 = x : helper(y: xs)
            | otherwise = x : []        
-- main = do
