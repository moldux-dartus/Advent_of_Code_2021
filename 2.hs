main = do
  readings <- readFile "input.txt"
  let rList = map (\x -> read x :: Integer) $ words readings
      sumList = map (\(a,b,c) -> a + b + c) $ zip3 rList (tail rList) (tail $ tail rList)
  print $ length $ filter (\(a,b) -> a < b) $ zip sumList (tail sumList)
