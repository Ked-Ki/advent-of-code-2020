module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many' $ do
  n <- decimal
  skipSpace
  return n

------------ TYPES ------------
type Input = [Int]

type OutputA = Int
partA :: R.Part Input OutputA
partA = R.defaultPart "Part A" solveA

type OutputB = Int
partB :: R.Part Input OutputB
partB = R.defaultPart "Part B" solveB

------------ PART A ------------
solveA :: Input -> OutputA
solveA ipt = fromJust $ findMatch (makeMap 2020 ipt) ipt

findMatch :: Map Int Int -> [Int] -> Maybe Int
findMatch m (x:xs) = maybe (findMatch m xs) (\x' -> Just (x*x')) (Map.lookup x m)
findMatch _ [] = Nothing

makeMap :: Int -> [Int] -> Map Int Int
makeMap target = Map.fromList . map mkPair 
  where
    mkPair x = (target-x, x)

------------ PART B ------------
solveB :: Input -> OutputB
solveB = fromJust . findTriple

findTriple :: [Int] -> Maybe Int
findTriple (x:xs) = maybe (findTriple xs) (\x' -> Just (x*x')) $ findTwoMatches (2020-x) xs
findTriple [] = Nothing

findTwoMatches :: Int -> [Int] -> Maybe Int
findTwoMatches target xs = findMatch (makeMap target xs) xs
