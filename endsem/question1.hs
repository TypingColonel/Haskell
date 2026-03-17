main :: IO()

printAll :: [Int] -> String
printAll [] = ""
printAll (x: y: xs)
    | y == 0 = show (x) ++ " " ++ printAll xs
    | otherwise = show x ++ "x" ++ "^" ++ show y ++ " " ++ printAll xs

addValuse :: [Int] -> [Int]
addValuse [] = []
addValuse (x: y: xs) 
    | x * y == 0 = addValuse xs
    | otherwise = x * y : y - 1 : addValuse xs


main = do
    putStrLn(printAll [1, 2, 4, 5, 6, 7, 9, 0])
    print(addValuse [1, 2, 4, 5, 6, 7, 9, 0])