import System.IO

main =
  withFile
    "input"
    ReadMode
    ( \handle -> do
        contents <- hGetContents handle
        putStr (show (sol1 (lines contents)))
        putStr "\n"
        --putStr (show (sol2 (parseInput (head (lines contents)))))
        putStr "\n"
    )

sol1 :: [String] -> Int
sol1 outputs = sum (map (length . filter isEasy . parseInput) outputs)

isEasy :: String -> Bool
isEasy l = (length l == 2) || (length l == 3) || (length l == 4) || (length l == 7)

parseInput :: String -> [String]
parseInput line  = splitSpace (tail (dropWhile (/= '|') line))

splitSpace :: String -> [String]
splitSpace [] = []
splitSpace (' ' : xs) = splitSpace xs
splitSpace s = takeWhile (/= ' ') s : splitSpace (dropWhile (/= ' ') s)
