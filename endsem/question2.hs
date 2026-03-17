main :: IO()

allPlateaus :: [Int] -> [[Int]]
allPlateaus [] = []
allPlateaus (x: xs) = (x: val) : allPlateaus next
    where
        val = takeWhile (==x) xs
        next = dropWhile (==x) xs

main = do
    putStrLn (show (allPlateaus [3, 3, 3, 2, 2, 5]))