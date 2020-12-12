module Days.Day12 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Control.Applicative.Combinators ((<|>), many)
import Data.Functor (($>))
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many $ instrParser <* endOfLine
  where
    instrParser :: Parser Instruction
    instrParser = choice [ Cardinal North <$> ("N" *> decimal)
                         , Cardinal South <$> ("S" *> decimal) 
                         , Cardinal East  <$> ("E" *> decimal)
                         , Cardinal West  <$> ("W" *> decimal)
                         , ("L90"  <|> "R270") $> TurnLeft
                         , ("L180" <|> "R180") $> TurnAround
                         , ("L270" <|> "R90" ) $> TurnRight
                         , Forward <$> ("F" *> decimal)
                         ]

------------ TYPES ------------
type Input = [Instruction]

data Direction = North
               | South
               | East
               | West
               deriving (Show,Eq)

data Instruction = Cardinal Direction Int
                 | TurnLeft
                 | TurnAround
                 | TurnRight
                 | Forward Int
                 deriving Show

type Pos = (Int,Int) -- (east,north)


-- vector addition, subtraction, & scalar multiplication for Pos
(.-.) :: Pos -> Pos -> Pos
(x1,y1) .-. (x2,y2) = (x1-x2,y1-y2)

(.+.) :: Pos -> Pos -> Pos
(x1,y1) .+. (x2,y2) = (x1+x2,y1+y2)

(.*) :: Int -> Pos -> Pos
s .* (x,y) = (s*x,s*y)


data Boat = Boat { facing :: Direction
                 , pos :: Pos
                 }
                 deriving Show

initBoat :: Boat
initBoat = Boat East (0,0)

data Waypoint = Waypoint { waypoint :: Pos
                         , boat :: Pos
                         }

initWaypoint :: Waypoint
initWaypoint = Waypoint (10,1) (0,0)

type OutputA = Pos

type OutputB = Pos

------------ PART A ------------
partA :: Input -> OutputA
partA = pos . foldl runInstr initBoat
  where
    runInstr :: Boat -> Instruction -> Boat
    runInstr b@Boat{..} (Cardinal North y) = b { pos = pos .+. (0,y)}
    runInstr b@Boat{..} (Cardinal South y) = b { pos = pos .-. (0,y)}
    runInstr b@Boat{..} (Cardinal East  x) = b { pos = pos .+. (x,0)}
    runInstr b@Boat{..} (Cardinal West  x) = b { pos = pos .-. (x,0)}
    runInstr b@Boat{..} TurnLeft = b { facing = l facing }
      where
        l = \case North -> West; West -> South; South -> East; East -> North
    runInstr b@Boat{..} TurnAround = b { facing = a facing }
      where
        a = \case North -> South; West -> East; South -> North; East -> West
    runInstr b@Boat{..} TurnRight = b { facing = r facing }
      where 
        r = \case North -> East; West -> North; South -> West; East -> South
    runInstr b@Boat{..} (Forward x) = runInstr b (Cardinal facing x)

------------ PART B ------------
partB :: Input -> OutputB
partB = boat . foldl runInstr initWaypoint
  where
    runInstr :: Waypoint -> Instruction -> Waypoint
    runInstr w@Waypoint{..} (Cardinal North y) = w { waypoint = waypoint .+. (0,y)}
    runInstr w@Waypoint{..} (Cardinal South y) = w { waypoint = waypoint .-. (0,y)}
    runInstr w@Waypoint{..} (Cardinal East  x) = w { waypoint = waypoint .+. (x,0)}
    runInstr w@Waypoint{..} (Cardinal West  x) = w { waypoint = waypoint .-. (x,0)}
    runInstr w@Waypoint{..} TurnLeft = w { waypoint = rotatePosL waypoint }
    runInstr w@Waypoint{..} TurnAround = w { waypoint = (-1) .* waypoint }
    runInstr w@Waypoint{..} TurnRight = w { waypoint = rotatePosR waypoint }
    runInstr w@Waypoint{..} (Forward x) = w { boat = boat .+. (x .* waypoint) }

    rotatePosL :: Pos -> Pos
    rotatePosL (x,y) = (-y,x)

    rotatePosR :: Pos -> Pos
    rotatePosR (x,y) = (y,-x)
