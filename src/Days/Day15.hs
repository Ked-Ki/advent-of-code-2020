module Days.Day15 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Program.RunDay as R
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy decimal ","

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
solveA ipt = runGame ipt !! 2019

runGame :: Input -> [Int]
runGame ipt = ipt ++ go (length ipt + 1) (last ipt) (Map.fromList $ zip (init ipt) [1..])
  where
    go :: Int -> Int -> Map Int Int -> [Int]
    go turn last m = case Map.lookup last m of
                       Just x -> let newNum = turn - x - 1
                                  in newNum : go (turn + 1) newNum newMap
                       Nothing -> 0 : go (turn + 1) 0 newMap
      where 
        newMap = Map.insert last (turn - 1) m

------------ PART B ------------
solveB :: Input -> OutputB
solveB ipt = runGame ipt !! 29999999
