main :: IO ()
main = do
    content <- readFile "input.txt"
    print $ foldl (+) 0 (map convert $ lines content)

convert :: String -> Int
convert = read . filter (/= '+')