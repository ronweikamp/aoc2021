import System.IO
--import Data.Char

main = do
    withFile "input" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr (show (sol (map readBits (lines contents))))
        putStr "\n"
        putStr (show (sol2 (map readBits (lines contents)))))

gammaRate :: [[Bit]] -> [Bit]
gammaRate numbers = foldl (\gr i -> gr ++ [mostCommon (getColumn i numbers)]) [] [0..length(head numbers) - 1]

toDecimal :: [Bit] -> Int
toDecimal bits =  sum [if b == One then 2^i else 0 | (b,i) <- zip bits (reverse [0..(length bits - 1)])]

getOxigen :: [[Bit]] -> [Bit]
getOxigen bnumbers = getOxigenRecursive bnumbers 0

getOxigenRecursive :: [[Bit]] -> Int -> [Bit]
getOxigenRecursive [bits] _ = bits
getOxigenRecursive bnumbers i = getOxigenRecursive (filter (\bits -> mostCommonOne (getColumn i bnumbers) == bits!!i) bnumbers) (mod (i+1) (length (head bnumbers)))

getCO2 :: [[Bit]] -> [Bit]
getCO2 bnumbers = getCO2Recursive bnumbers 0

getCO2Recursive :: [[Bit]] -> Int -> [Bit]
getCO2Recursive [bits] _ = bits
getCO2Recursive bnumbers i = getCO2Recursive (filter (\bits -> leastCommon (getColumn i bnumbers) == bits!!i) bnumbers) (mod (i+1) (length (head bnumbers)))

readBits :: String -> [Bit]
readBits = map readBit

readBit :: Char -> Bit
readBit '1' = One
readBit '0' = Zero
readBit _ = error "unsupported char"

sol :: [[Bit]] -> Int
sol bnumbers = toDecimal (gammaRate bnumbers) * toDecimal (map flipBit (gammaRate bnumbers))

sol2 :: [[Bit]] -> Int
sol2 bnumbers = toDecimal (getOxigen bnumbers) * toDecimal (getCO2 bnumbers)

mostCommon :: [Bit] -> Bit
mostCommon bits = if sumTrues (map (== One) bits) > sumTrues (map (== Zero) bits) then One else Zero

mostCommonOne :: [Bit] -> Bit
mostCommonOne bits = if sumTrues (map (== One) bits) >= sumTrues (map (== Zero) bits) then One else Zero

leastCommon :: [Bit] -> Bit
leastCommon bits = if sumTrues (map (== One) bits) < sumTrues (map (== Zero) bits) then One else Zero

getColumn :: Int -> [[Bit]] -> [Bit]
getColumn i = map (!!i)

sumTrues :: [Bool] -> Int
sumTrues = foldl (\i v -> if v then i + 1 else i) 0

flipBit :: Bit -> Bit
flipBit Zero = One
flipBit One = Zero

data Bit = Zero | One deriving (Eq)
