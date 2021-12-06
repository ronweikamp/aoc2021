import System.IO
import Data.List (group, sort)
import Debug.Trace

main =
    withFile "testinput" ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr (show (sol1 (parseInput (head (lines contents)))))
    putStr "\n"
    putStr "\n")

sol1 :: [Int] -> Int
sol1 s = sol1Rec s 80

sol1Rec :: [Int] -> Int -> Int
sol1Rec s 0 = length s
sol1Rec s i = sol1Rec (step s) (i - 1)

--sol1Rec :: [Int] -> Int -> Int
--sol1Rec s 0 = trace (show s) (length s)
--sol1Rec s i = trace (show s) (sol1Rec (step s) (i - 1))

step :: [Int] -> [Int]
step state = let newstateexisting = map (\i -> if i > 6 then i-1 else mod (i-1) 7) state in
                 newstateexisting ++ replicate (countNew state newstateexisting) 8

countNew :: [Int] -> [Int] -> Int
countNew state newstateexisting = length (filter (== 6) [s0 + s1 | (s0, s1) <- zip state newstateexisting])

parseInput :: String -> [Int]
parseInput s = map read (splitComma s)

splitComma :: String -> [String]
splitComma [] = []
splitComma (',':xs) = splitComma xs
splitComma s = takeWhile (/= ',') s : splitComma (dropWhile (/= ',') s)
