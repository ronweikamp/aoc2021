import System.IO
import Data.Char

main = do
    withFile "input" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr (show (sol (map ((read :: String -> Move) . capitalize) (lines contents))))
        putStr "\n"
        putStr (show (sol2 (map ((read :: String -> Move) . capitalize) (lines contents)))))

readInt :: String -> Int
readInt = read

sol :: [Move] -> Int
sol moves = sum (map (getMovement Vertical) moves) * sum (map (getMovement Horizontal) moves)

sol2 :: [Move] -> Int
sol2 moves =  sum [aim * forward | (aim, forward) <- zip (aims moves []) (map (getMovement Horizontal) moves)] * sum (map (getMovement Horizontal) moves)

aims :: [Move] -> [Int] -> [Int]
aims ms as = tail (foldl (\as m -> as ++ [last as + getMovement Vertical m]) [0] ms)

data Move = Up Int | Down Int | Forward Int deriving (Show, Read)
data Direction = Horizontal | Vertical deriving (Show)

getMovement :: Direction -> Move -> Int
getMovement Vertical (Up m) = -m
getMovement Vertical (Down m) = m
getMovement Horizontal (Forward m) = m
getMovement _ _ = 0

capitalize :: String -> String
capitalize (head:tail) = toUpper head : tail
capitalize [] = []
