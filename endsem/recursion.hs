maxrec :: (Ord a) => [a] -> a
maxrec [] = error "Empty list"
maxrec [x] = x
maxrec (x: xs) 
    | x > maxrec xs = x
    | otherwise maxrec xs

clearMaxRec [] = maxrec []
clearMaxRec [x] = maxrec [x]
clearMaxRec (x: xs) = max x (clearMaxRec xs)

replicate n x
    | n <= 0 = []
    | otherwise = x : replicate (n - 1) xs

myTake n _
    | n <= 0 = []
myTake _ [] = []
myTake n (x: xs) = x : myTake (n - 1) xs

myreverse [] = []
myreverse (x: xs) = myreverse xs ++ [x]

myelement [] _ = False
myelement (x: xs) y 
    | x == y = True
    | otherwise = myelement xs

-- currying => Every function in haskell takes only one argument
-- functions as parameters

applyTwice2 :: (a -> a) -> a -> a
applyTwice2 f x = f (f x)

-- myfilter :: (a -> )

-- custom zip with function

zipWith (*) [1, 2, 4, 4] [5, 12, 51]

customZipWith _ [] _ = []
customZipWith _ _ [] = []
customZipWith f (x: xs) (y: ys) = (f (x) (y)) ++ customZipWith f xs ys
 
-- custom map function
customMapFunction _ [] = []
customMapFunction f (x: xs) = f x ++ customMapFunction f xs

-- take while function

takeWhile (< 100) [1 ..]

-- if else statement

if x `mod` 2 == 0 then "Even" else "odd"

-- lambda functions 

\x y -> x + y + 3

-- foldl and foldr 

foldl (+) 0 [10, 20, 120]

main :: IO()

main = do 
    -- show()