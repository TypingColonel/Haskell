main :: IO()

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort [x: xs] = 
    quickSort [y | y <- xs, x > y]
    ++ [x] ++
    quickSort [y | y <- xs, y > x]

consecutiveDecending :: [Int] -> [[Int]]
consecutiveDecending [] = []
consecutiveDecending [x] = [[x]]
consecutiveDecending (x: y: xs)
    | x - 1 == y = consecutiveDecending ()


