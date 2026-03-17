conform :: String -> String -> Bool
conform [] _ = True
conform _ [] = False
conform (x: ip) (y: ips) 
    | x == y = conform ip ips
    | otherwise = False

getValidIps :: String -> [String] -> [String]
getValidIps _ [] = []
getValidIps ip (x: ips) 
    | conform ip x = x : getValidIps ip ips
    | otherwise = getValidIps ip ips

main :: IO ()
main = do 
    putStrLn(show (getValidIps "172.161" ["172.161.92.33","172.160.72.11","172.161.88.55","171.161.17.16"]))