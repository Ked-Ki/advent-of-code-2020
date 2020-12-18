module Days.Day11 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Util.Util as U
import Util.Parsers (coordinateParser,Coordinates(..), overM)

import qualified Program.RunDay as R
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser (\case 'L' -> Just Vacant; '#' -> Just Occupied; _ -> Nothing) 0

------------ TYPES ------------
type Input = Coordinates Seat

data Seat = Occupied | Vacant
  deriving (Show, Eq)

type SeatUpdater = Coordinates Seat -> (Int,Int) -> Seat -> Seat

type OutputA = Int
partA :: R.Part Input OutputA
partA = R.defaultPart "Part A" solveA

type OutputB = Int
partB :: R.Part Input OutputB
partB = R.defaultPart "Part B" solveB

------------ PART A ------------
solveA :: Input -> OutputA
solveA = runSim updaterA
  where
    updaterA :: SeatUpdater
    updaterA m c Vacant   = if null (getAdjacents c m)
                               then Occupied
                               else Vacant
    updaterA m c Occupied = if length (getAdjacents c m) >= 4
                               then Vacant
                               else Occupied

    getAdjacents :: (Int,Int) -> Coordinates Seat -> [Seat]
    getAdjacents (x,y) c = filter (==Occupied) $ catMaybes $ flip Map.lookup (coord_map c) <$>
      [(x',y') | x' <- [x-1,x,x+1], y' <- [y-1,y,y+1], x' /= x || y' /= y]


runSim :: SeatUpdater -> Coordinates Seat -> Int
runSim f = Map.size . Map.filter (==Occupied) . coord_map . go
  where
    go :: Coordinates Seat -> Coordinates Seat
    go m = let next = updateAllSeats f m
            in if Map.elems (coord_map next) == Map.elems (coord_map m)
                  then next
                  else go next

updateAllSeats :: SeatUpdater -> Coordinates Seat -> Coordinates Seat
updateAllSeats f c = overM (Map.mapWithKey (f c)) c


------------ PART B ------------
type Dir = (Int,Int)

solveB :: Input -> OutputB
solveB c = runSim updaterB c
  where
    updaterB :: SeatUpdater
    updaterB c seat Vacant   = if getAdjacentOccupiedCount c seat == 0
                                  then Occupied
                                  else Vacant
    updaterB c seat Occupied = if getAdjacentOccupiedCount c seat >= 5
                                  then Vacant
                                  else Occupied

    getAdjacentOccupiedCount :: Coordinates Seat -> (Int,Int) -> Int
    getAdjacentOccupiedCount Coordinates{..} seat = length $ filter (Occupied==) adjacentSeats
      where
        adjacentSeats :: [Seat]
        adjacentSeats = catMaybes $ flip Map.lookup coord_map <$> adjacentCoords
        adjacentCoords :: [(Int,Int)]
        adjacentCoords = fromJust $ Map.lookup seat adjacencyMap

    -- memoized version of getAdjacents
    adjacencyMap :: Map (Int,Int) [(Int,Int)]
    adjacencyMap = Map.fromList $ zip all_seat_coords (map (getAdjacents c) all_seat_coords) 
      where
        all_seat_coords = Map.keys (coord_map c)

    -- gets the coordinates of all visible seats from a coordinate
    getAdjacents :: Coordinates Seat -> (Int,Int) -> [(Int,Int)]
    getAdjacents c (x,y) = mapMaybe (castRay c (x,y)) directions

    -- returns closest coordinate with a seat (occupied or vacant) in a particular direction
    castRay :: Coordinates Seat -> (Int,Int) -> Dir -> Maybe (Int,Int)
    castRay Coordinates{..} (x,y) (dx,dy) = fst <$> find (isJust . snd) lookups
      where
        lookups :: [((Int,Int),Maybe Seat)]
        lookups = zip all_coords $ flip Map.lookup coord_map <$> all_coords
        all_coords = zip (U.enumInBounds (x+dx) dx x_bound) (U.enumInBounds (y+dy) dy y_bound)

    directions :: [Dir]
    directions = [(x',y') | x' <- [-1,0,1], y' <- [-1,0,1], x' /= 0 || y' /= 0]
