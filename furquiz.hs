f :: Int -> Int

f n = g n
    where 
        g 0 = -1
        g x = x * g (x - 1)

main = do
    print(f (-1)) 