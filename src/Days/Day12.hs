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

overPos :: (Pos -> Pos) -> Boat -> Boat
overPos f b@Boat{..} = b { pos = f pos }

overFacing :: (Direction -> Direction) -> Boat -> Boat
overFacing f b@Boat{..} = b { facing = f facing } 

initBoat :: Boat
initBoat = Boat East (0,0)

data Waypoint = Waypoint { waypoint :: Pos
                         , boat :: Pos
                         }

overWaypoint :: (Pos -> Pos) -> Waypoint -> Waypoint
overWaypoint f w@Waypoint{..} = w { waypoint = f waypoint }

overBoat :: (Pos -> Pos) -> Waypoint -> Waypoint
overBoat f w@Waypoint{..} = w { boat = f boat }

initWaypoint :: Waypoint
initWaypoint = Waypoint (10,1) (0,0)

type OutputA = Pos

type OutputB = Pos

------------ PART A ------------
partA :: Input -> OutputA
partA = pos . foldl runInstr initBoat
  where
    runInstr :: Boat -> Instruction -> Boat
    runInstr b (Cardinal North y) = overPos (.+. (0,y)) b 
    runInstr b (Cardinal South y) = overPos (.-. (0,y)) b
    runInstr b (Cardinal East  x) = overPos (.+. (x,0)) b
    runInstr b (Cardinal West  x) = overPos (.-. (x,0)) b

    runInstr b TurnLeft = overFacing l b
      where
        l = \case North -> West; West -> South; South -> East; East -> North
    runInstr b TurnAround = overFacing a b
      where
        a = \case North -> South; West -> East; South -> North; East -> West
    runInstr b TurnRight = overFacing r b
      where 
        r = \case North -> East; West -> North; South -> West; East -> South

    runInstr b@Boat{..} (Forward x) = runInstr b (Cardinal facing x)

------------ PART B ------------
partB :: Input -> OutputB
partB = boat . foldl runInstr initWaypoint
  where
    runInstr :: Waypoint -> Instruction -> Waypoint
    runInstr w              (Cardinal North y) = overWaypoint (.+. (0,y)) w
    runInstr w              (Cardinal South y) = overWaypoint (.-. (0,y)) w
    runInstr w              (Cardinal East  x) = overWaypoint (.+. (x,0)) w
    runInstr w              (Cardinal West  x) = overWaypoint (.-. (x,0)) w
    runInstr w               TurnLeft          = overWaypoint rotatePosL w
    runInstr w               TurnAround        = overWaypoint ((-1) .*) w
    runInstr w               TurnRight         = overWaypoint rotatePosR w
    runInstr w@Waypoint{..} (Forward x)        = overBoat (.+. (x .* waypoint)) w

    rotatePosL :: Pos -> Pos
    rotatePosL (x,y) = (-y,x)

    rotatePosR :: Pos -> Pos
    rotatePosR (x,y) = (y,-x)
