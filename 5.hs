import Data.Char

type Reading = String
type Position = Integer

main = do
  readings <- readFile "input3.txt"
  let readingList = words readings
      ep = toDec $ epsilon $ freqList readingList
      g  = toDec $ gamma $ freqList readingList
  print $ "Epsilon: " ++ (show ep) ++ "  Gamma: " ++ (show g)

posCount :: [Reading] -> Position -> Integer
--posFreq [] _ = []
posCount rs n = foldl1 (+)
               $ map (toInteger . digitToInt)
               $ map (!! (fromInteger n)) rs

freqList :: [Reading] -> [Integer]
freqList rl = map (posCount rl) (map toInteger [0..(length $ tail $ head rl)])

gamma :: [Integer] -> String
gamma = concat . map show . map (\x -> if x >= 500 then 1 else 0)

epsilon :: [Integer] -> String
epsilon = concat . map show . map (\x -> if x < 500 then 1 else 0)

toDec :: String -> Integer
toDec = foldl (\acc x -> acc * 2 + (toInteger $ digitToInt x)) 0
