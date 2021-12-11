import Data.List (intersect)
import System.IO

main =
  withFile
    "input"
    ReadMode
    ( \handle -> do
        contents <- hGetContents handle
        putStr (show (sol1 (lines contents)))
        putStr "\n"
        putStr (show (sol2 (lines contents)))
        putStr "\n"
    )

sol1 :: [String] -> Int
sol1 outputs = sum (map (length . filter isEasy . parseInput) outputs)

sol2 :: [String] -> Int
sol2 input =
  sum
    ( [ readInt
          (concatMap (show . decodeNumber signals) numbers)
        | (signals, numbers) <-
            zip
              (map parseFirstInput input)
              (map parseInput input)
      ]
    )

readInt :: String -> Int
readInt = read

countOverlap :: String -> String -> Int
countOverlap s1 s2 = length (s1 `intersect` s2)

decodeNumber :: [String] -> String -> Int
decodeNumber numbers number = case length number of
  2 -> 1
  4 -> 4
  3 -> 7
  7 -> 8
  6 -> sixOrNineOrZero numbers number
  _ -> twoOrFiveOrThree numbers number

getEasy :: [String] -> Int -> String
getEasy numbers 1 = head (filter (\s -> length s == 2) numbers)
getEasy numbers 4 = head (filter (\s -> length s == 4) numbers)
getEasy numbers 7 = head (filter (\s -> length s == 3) numbers)
getEasy numbers 8 = head (filter (\s -> length s == 7) numbers)
getEasy numbers _ = error "not easy"

sixOrNineOrZero :: [String] -> String -> Int
sixOrNineOrZero numbers number
  | countOverlap number (getEasy numbers 1) == 1 = 6
  | countOverlap number (getEasy numbers 4) == 4 = 9
  | otherwise = 0

twoOrFiveOrThree :: [String] -> String -> Int
twoOrFiveOrThree numbers number
  | countOverlap number (getEasy numbers 4) == 2 = 2
  | countOverlap number (getEasy numbers 1) == 2 = 3
  | otherwise = 5

isEasy :: String -> Bool
isEasy l = length l == 2 || length l == 3 || length l == 4 || length l == 7

parseInput :: String -> [String]
parseInput line = splitSpace (tail (dropWhile (/= '|') line))

parseFirstInput :: String -> [String]
parseFirstInput line = splitSpace (takeWhile (/= '|') line)

splitSpace :: String -> [String]
splitSpace [] = []
splitSpace (' ' : xs) = splitSpace xs
splitSpace s = takeWhile (/= ' ') s : splitSpace (dropWhile (/= ' ') s)
