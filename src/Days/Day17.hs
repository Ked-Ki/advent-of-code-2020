{-# LANGUAGE ScopedTypeVariables #-}
module Days.Day17 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Program.RunDay as R
import Data.Attoparsec.Text
import Util.Parsers (prettyPrintCoords, Coordinates(..), coord_map, coordinateParser)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coord_map <$> twoDSlice
  where
    twoDSlice = coordinateParser (\case '#' -> Just (); _ -> Nothing) 0

------------ TYPES ------------
type Input = Map (Int,Int) ()

type Coord3 = (Int,Int,Int)

type Coord4 = (Int,Int,Int,Int)

type Grid c = Map c () -- a value indicates active, absent indicates inactive

type OutputA = Grid Coord3
partA :: R.Part Input OutputA
partA = R.Part { name = "Part A"
               , solve = solveA
               , showSol = ppGrid
               , toInt = toInteger . Map.size
               }

type OutputB = Int
partB :: R.Part Input OutputB
partB = R.defaultPart "Part B" solveB

------------ PART A ------------
solveA :: Input -> OutputA
solveA g = runSim getAdj3d threeDGrid !! 6
  where
    threeDGrid = Map.mapKeys (\(x,y) -> (x,y,0)) g

    getAdj3d :: Coord3 -> Set Coord3
    getAdj3d (x,y,z) =
        Set.fromList $ [(x',y',z') | x' <- adj x, y' <- adj y, z' <- adj z,
                                     x /= x' || y /= y' || z /= z']

    adj i = [i-1,i,i+1]

runSim :: (Ord c) => (c -> Set c) -> Grid c -> [Grid c]
runSim getAdjCoords = iterate (update getAdjCoords)

update :: forall c. (Ord c) => (c -> Set c) -> Grid c -> Grid c
update getAdjCoords g = foldr alterCoord g csToConsider
  where
    csToConsider :: Set c
    csToConsider = Set.unions $ Set.fromList activeCs:(getAdjCoords <$> activeCs)
      where
        activeCs = Map.keys g

    alterCoord :: c -> Grid c -> Grid c
    alterCoord c = Map.alter (updateCoord c) c

    updateCoord :: c -> Maybe () -> Maybe ()
    updateCoord c cur =
        case cur of
          Just _ -> if adjCnt == 2 || adjCnt == 3
                       then Just ()
                       else Nothing
          Nothing -> if adjCnt == 3
                        then Just ()
                        else Nothing
        where
          adjCnt = cntActiveAdj c

    cntActiveAdj :: c -> Int
    cntActiveAdj = Set.size . Set.filter (`Map.member` g) . getAdjCoords


-- pretty print the 3d grid, for debugging/admiring pretty ascii
ppGrid :: Grid Coord3 -> String
ppGrid g = unlines $ map (`prettyPrintCoords` \case Just _ -> '#'; Nothing -> '.') toSlices
  where
    toSlices :: [Coordinates ()]
    toSlices = map makeSlice $ uncurry enumFromTo (getBounds selZ)

    makeSlice :: Int -> Coordinates ()
    makeSlice z = Coordinates { coord_map = dropZCoord $ Map.filterWithKey (\(_,_,z') _ -> z' == z) g
                              , x_bound = getBounds selX
                              , y_bound = getBounds selY
                              }

    dropZCoord :: Grid Coord3 -> Map (Int,Int) ()
    dropZCoord = Map.mapKeys (\(x,y,_) -> (x,y))

    getBounds :: (Coord3 -> Int) -> (Int,Int)
    getBounds sel = (minimum is,maximum is)
      where
        is = map sel $ Map.keys g

    selX (x,_,_) = x
    selY (_,y,_) = y
    selZ (_,_,z) = z


------------ PART B ------------
solveB :: Input -> OutputB
solveB g = Map.size $ runSim getAdj4d fourDGrid !! 6
  where
    fourDGrid = Map.mapKeys (\(x,y) -> (x,y,0,0)) g

    getAdj4d :: Coord4 -> Set Coord4
    getAdj4d (x,y,z,w) =
        Set.fromList $ [(x',y',z',w') | x' <- adj x, y' <- adj y, z' <- adj z, w' <- adj w,
                                        x /= x' || y /= y' || z /= z' || w /= w']

    adj i = [i-1,i,i+1]
