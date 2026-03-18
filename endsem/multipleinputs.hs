main :: IO ()
main = do
    putStrLn "Enter integers separated by spaces:"
    input <- getLine
    let numbers = map read (words input) :: [Int]
    putStrLn "You entered:"
    print numbers
