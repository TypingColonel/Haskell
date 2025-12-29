main :: IO()

exponentialSequence x 
    | x >= 1000 || x == 0 || x == 1 = []
    | even x = x : exponentialSequence (x * x)
    | otherwise = x : exponentialSequence (x * x * x)

quickSort :: [Int] -> [Int]
quickSort (x:xs) = 
    quickSort ([y | y <- xs, y <= x] ++ [x] ++ [y | y <- xs, y > x])

main = do
    putStrLn "Enter a number"
    name <- readLn :: IO Int
    print(exponentialSequence name)
