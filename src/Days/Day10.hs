module Days.Day10 (runDay) where

{- ORMOLU_DISABLE -}
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import qualified Program.RunDay as R 
import Data.Attoparsec.Text
import Control.Applicative.Combinators (many)
import Data.Maybe (fromJust)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = foldr IntSet.insert IntSet.empty <$> many (decimal <* endOfLine)

------------ TYPES ------------
type Input = IntSet

-- map of diff size to counts. puzzle asks for counts of 1 & 3 multiplied together
type OutputA = IntMap Int
partA :: R.Part Input OutputA
partA = R.Part { name = "Part A"
               , solve = solveA
               , showSol = show
               , toInt = \m -> toInteger . fromJust $ (*) <$> IntMap.lookup 1 m <*> IntMap.lookup 3 m
               }

type OutputB = Maybe Int
partB :: R.Part Input OutputB
partB = R.Part { name = "Part B"
               , solve = solveB
               , showSol = show
               , toInt = toInteger . fromJust
               }

------------ PART A ------------
solveA :: Input -> OutputA
solveA adapterSet = snd $ IntSet.foldl update (0,initialMap) setWithPhone
  where
    update (prev,map) cur = (cur, IntMap.update (Just . (+1)) (cur-prev) map)
    initialMap = IntMap.fromList [(1,0),(2,0),(3,0)]
    setWithPhone = IntSet.insert (IntSet.findMax adapterSet + 3) adapterSet
    
    
------------ PART B ------------
solveB :: Input -> OutputB
solveB adapterSet = IntMap.lookup 0 $ runDP (maxElem-1) (IntMap.singleton maxElem 1)
  where
   runDP :: Int -> IntMap Int -> IntMap Int
   runDP cur m 
     | cur >= 0 = runDP (cur-1) $ if IntSet.member cur withEnds 
                                     then IntMap.insert cur newVal m
                                     else m
     | otherwise = m
        where
          newVal = IntMap.findWithDefault 0 (cur+3) m +
                   IntMap.findWithDefault 0 (cur+2) m +
                   IntMap.findWithDefault 0 (cur+1) m 

   withEnds = IntSet.insert 0 $ IntSet.insert maxElem adapterSet
   maxElem = IntSet.findMax adapterSet + 3
