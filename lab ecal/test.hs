import Data.Char(toUpper)
main :: IO()

main = do
    print(map (*2) (filter even [1,2,3,4,5,6]))
    print(map (^2) (filter (> 5) [2,6,8,3,10]))
    print(map (map toUpper) (filter (\x -> length x > 4) ["cat","banana","pear","pineapple"]))
    print(map (+3) (filter (< 10) (filter (odd) ([5,11,3,14,7,9,15]))))
    -- print()