module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Data.Map.Strict as Map
import Data.Maybe
import Util.Parsers (coordinateParser, Coordinates(..))

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser toSquare 0
  where
    toSquare '#' = Just ()
    toSquare _ = Nothing

------------ TYPES ------------
type Input = Coordinates () -- map will contain (x,y) iff (x,y) is a tree

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = countTrees 3 1

countTrees :: Int -> Int -> Input -> Int
countTrees delta_x delta_y Coordinates{..} = 
  length $ filter (isJust . flip Map.lookup coord_map) traversed
  where
    traversed = zip (map (`mod` (hx+1)) [lx,delta_x..]) [ly,delta_y..hy]
    (lx,hx) = x_bound
    (ly,hy) = y_bound


------------ PART B ------------
partB :: Input -> OutputB
partB c = product $ map (\(x,y) -> countTrees x y c) slopes
  where
    slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
