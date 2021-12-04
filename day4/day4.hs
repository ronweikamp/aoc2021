import System.IO

main =
    withFile "input" ReadMode (\handle -> do
    contents <- hGetContents handle
    let (n, card) = playBingo (parseNumbers (lines contents)) (parseBingoCards (lines contents)) in
        putStr (show (n * cardValue card))
    putStr "\n"
    let (n, card) = playBingoLast (parseNumbers (lines contents)) (parseBingoCards (lines contents)) in
        putStr (show (n * cardValue card))
    putStr "\n")

playBingo :: [Int] -> [BingoCard] -> (Int, BingoCard)
playBingo numbers cards = let
                            number = head numbers
                            markedCards = markCards number cards
                          in
                              if any isBingo markedCards then (number, head (filter isBingo markedCards)) else playBingo (tail numbers) markedCards

playBingoLast :: [Int] -> [BingoCard] -> (Int, BingoCard)
playBingoLast numbers cards = let
                                number = head numbers
                                markedCards = markCards (head numbers) cards
                           in
                              if all isBingo markedCards then (number, head (markCards number (filter (not . isBingo) cards))) else playBingoLast (tail numbers) markedCards

markCards :: Int -> [BingoCard] -> [BingoCard]
markCards i = map (map (map (\field -> if field == Field i then Marked else field)))

cardValue :: BingoCard -> Int
cardValue bc = sum (map sumRow bc)

sumRow :: [BingoField] -> Int
sumRow row = sum (map fieldValue row)

fieldValue :: BingoField -> Int
fieldValue (Field i) = i
fieldValue Marked = 0

isBingo :: BingoCard -> Bool
isBingo bc = any isBingoRow (bc ++ getColumns bc)

isBingoRow :: [BingoField] -> Bool
isBingoRow = all (== Marked)

getColumns :: BingoCard -> BingoCard
getColumns bc = [map (!!i) bc | i <- [0..4]]

parseNumbers :: [String] -> [Int]
parseNumbers input = map read (splitComma (head input))

splitSpace :: String -> [String]
splitSpace [] = []
splitSpace (' ':xs) = splitSpace xs
splitSpace s = takeWhile (/= ' ') s : splitSpace (dropWhile (/= ' ') s)

splitComma :: String -> [String]
splitComma [] = []
splitComma (',':xs) = splitComma xs
splitComma s = takeWhile (/= ',') s : splitComma (dropWhile (/= ',') s)

partitionEmptyLines :: [String] -> [[String]]
partitionEmptyLines [] = []
partitionEmptyLines ([]:xs) = partitionEmptyLines xs
partitionEmptyLines l = takeWhile (/= []) l : partitionEmptyLines (dropWhile (/= []) l)

parseBingoCards :: [String] -> [BingoCard]
parseBingoCards input = allIntsToField (allStringsToInt (map (map splitSpace) (partitionEmptyLines (tail input))))

allStringsToInt :: [[[String]]] -> [[[Int]]]
allStringsToInt = map (map (map read))

allIntsToField :: [[[Int]]] -> [[[BingoField]]]
allIntsToField = map (map (map Field))

parseBingoField :: Int -> BingoField
parseBingoField = Field

type BingoCard = [[BingoField]]
data BingoField = Field Int | Marked deriving (Show, Eq)
