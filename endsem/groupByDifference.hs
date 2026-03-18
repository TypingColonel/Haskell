-- main :: IO()

-- gbd _ [] = []
-- gbd k (x: y: xs) 
--     | y - x == k = x: gbd k (y: xs)
--     | otherwise = [x]: gbd k (y: xs)

-- main = do
--     print(gbd 5 [2, 7, 12, 20, 25])

-- groupIncreasing :: [Int] -> [[Int]]
-- groupIncreasing [] = []
-- groupIncreasing [x] = [[x]]
-- groupIncreasing (x: y: xs) 
--     | y > x = let
--                 (m: mx) = groupIncreasing (y: xs)
--               in (x: m): mx

--     | otherwise = [x] : groupIncreasing (y: xs)

-- groupByTrend :: [Int] -> [[Int]]
-- groupByTrend [] = []
-- groupByTrend [x] = [[x]]
-- groupByTrend (x: y: xs) 
--     | y - x == 0 = 

someRandomFunction xs = case xs of
    [] -> 0
    [x] -> x
    (x: _) -> _

