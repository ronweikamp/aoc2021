import System.IO
import Data.List (group, sort)

main =
    withFile "input" ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr (show (sol1 (map parseLine (lines contents))))
    putStr "\n"
    putStr (show (sol2 (map parseLine (lines contents))))
    putStr "\n")

sol1 :: [Line] -> Int
sol1 lines = let coords = findCoords (filter (\l -> isHorizontal l || isVertical l) lines) in
                 countCoordsThatExistMoreThanOnce coords

sol2 :: [Line] -> Int
sol2 lines = let coords = findCoords lines in
                 countCoordsThatExistMoreThanOnce coords

countCoordsThatExistMoreThanOnce :: [Coord] -> Int
countCoordsThatExistMoreThanOnce coords = length (filter (> 1) (map length (group . sort $ coords)))

findCoords :: [Line] -> [Coord]
findCoords = concatMap findCoordsOnLine

findCoordsOnLine :: Line -> [Coord]
findCoordsOnLine ((x1, y1), (x2, y2)) = [(x,y) | (x,y) <- zip (alwaysRange x1 x2) (alwaysRange y1 y2)]

alwaysRange :: Int -> Int -> [Int]
alwaysRange a b = [a, a + signum (b - a)..b]

isHorizontal :: Line -> Bool
isHorizontal ((x1,y1),(x2,y2)) = y1 == y2

isVertical :: Line -> Bool
isVertical ((x1,y1),(x2,y2)) = x1 == x2

parseLine :: String -> Line
parseLine s = ((read (takeWhile (/= ',') s), read (takeWhile (/= ' ') (tail (dropWhile (/= ',') s)))),
                (read (takeWhile (/= ',') (tail (dropWhile (/= '>') s))), read (tail (dropWhile (/= ',') (dropWhile (/= '>') s)))))

type Coord = (Int, Int)
type Line = ((Int, Int), (Int, Int))
