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
sol moves = sum (map (getMovement Vertical) moves) * sum (map (getMovement Horizontal) (filter isForward moves))

sol2 :: [Move] -> Int
sol2 moves =  sum [aim * forward | (aim, forward) <- zip (aims moves []) (map (getMovement Horizontal) moves)] * sum (map (getMovement Horizontal) moves)

aims :: [Move] -> [Int] -> [Int]
aims [] as = as
aims (m:ms) [] = aims ms [getMovement Vertical m]
aims (m:ms) as = aims ms (as ++ [last as + getMovement Vertical m])

data Move = Up Int | Down Int | Forward Int deriving (Show, Read)
data Direction = Horizontal | Vertical deriving (Show)

getMovement :: Direction -> Move -> Int
getMovement Vertical (Up m) = -m
getMovement Vertical (Down m) = m
getMovement Horizontal (Forward m) = m
getMovement _ _ = 0

isUp (Up _) = True
isUp _      = False

isDown (Down _) = True
isDown _        = False

isForward (Forward _) = True
isForward _           = False

capitalize :: String -> String
capitalize (head:tail) = toUpper head : tail
capitalize [] = []
