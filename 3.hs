data Submarine = Submarine (Integer, Integer) deriving Show

type Direction = String
type Distance = Integer
type Movement = (Direction, Distance)

move :: Submarine -> Movement -> Submarine
move (Submarine (x,y)) ("forward", i) = (Submarine ((x+i),y))
move (Submarine (x,y)) ("down", i)    = (Submarine (x,(y+i)))
move (Submarine (x,y)) ("up", i)      = (Submarine (x,(y-i)))
move _ _ = undefined

main = do
  course <- readFile "input2.txt"
  let moveList = parse $ words course
  print $ foldl move (Submarine (0,0)) moveList 

parse :: [String] -> [(Direction, Distance)]
parse (direction:distance:xs) = (direction, ( read distance :: Integer)) : parse xs
parse [] = []
