main = do
  readings <- readFile "input.txt"
  let rList = map (\x -> read x :: Integer) $ words readings
  putStrLn $ show $ length $ filter (\(a,b) -> a < b) $ zip rList (tail rList)
