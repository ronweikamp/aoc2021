import System.IO

main = do
    withFile "input" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr (show (sol2 (map readInt (lines contents)))))

readInt :: String -> Int
readInt = read

sol :: [Int] -> Int
sol l = sum (zipWith compareTuple l (tail l))

compareTuple :: Int -> Int -> Int
compareTuple a b = if b > a then 1 else 0

sol2 :: [Int] -> Int
sol2 l = let windows = [sum [a,b,c] | (a,b,c) <- zip3 l (tail l) (tail (tail l))] in
             sol windows
