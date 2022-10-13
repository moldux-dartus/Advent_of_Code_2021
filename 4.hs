data Submarine = Submarine (Integer, Integer, Integer) deriving Show

type Direction = String
type Distance = Integer
type Movement = (Direction, Distance)

move :: Submarine -> Movement -> Submarine
move (Submarine (x,y,a)) ("forward", i) = (Submarine ((x + i),(y + (a * i)),a))
move (Submarine (x,y,a)) ("down", i)    = (Submarine (x,y,(a + i)))
move (Submarine (x,y,a)) ("up", i)      = (Submarine (x,y,(a - i)))
move _ _ = undefined

main = do
  course <- readFile "input2.txt"
  let moveList = parse $ words course
  print $ foldl move (Submarine (0,0,0)) moveList 

parse :: [String] -> [Movement]
parse (direction:distance:xs) = (direction, ( read distance :: Integer)) : parse xs
parse [] = []
