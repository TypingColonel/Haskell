generateLine _ 0 = []
generatestars n k = let 
                        generateLine f = replicate f '*'
                    in 
                        [generateLine k]: generatestars n (k + 1)
