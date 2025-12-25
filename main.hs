main :: IO()

add :: () -> Int
add () = do 
    let b = 5
    b + 100

main = do
    print("Hello")
    print((+) 2 2 )
    print(show(True && False || True && (not True)))
    
    -- Concatination

    putStrLn ("my name ist " ++ "Sanjay")

    -- Cons operator

    