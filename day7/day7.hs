import System.IO

main =
  withFile
    "input"
    ReadMode
    ( \handle -> do
        contents <- hGetContents handle
        putStr (show (sol1 (parseInput (head (lines contents)))))
        putStr "\n"
        putStr (show (sol2 (parseInput (head (lines contents)))))
        putStr "\n"
    )

sol1 :: [Int] -> Int
sol1 hpositions = minimum [sum (map (\hp -> abs (hp - p)) hpositions) | p <- [minimum hpositions .. maximum hpositions]]

sol2 :: [Int] -> Int
sol2 hpositions = minimum [sum (map (\hp -> sum [1 .. (abs (hp - p))]) hpositions) | p <- [minimum hpositions .. maximum hpositions]]

parseInput :: String -> [Int]
parseInput s = map read (splitComma s)

splitComma :: String -> [String]
splitComma [] = []
splitComma (',' : xs) = splitComma xs
splitComma s = takeWhile (/= ',') s : splitComma (dropWhile (/= ',') s)
