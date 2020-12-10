module Days.Day10 (runDay) where

{- ORMOLU_DISABLE -}
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Control.Applicative.Combinators (many)
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

type OutputB = Maybe Int

------------ PART A ------------
partA :: Input -> OutputA
partA adapterSet = snd $ IntSet.foldl update (0,initialMap) setWithPhone
  where
    update (prev,map) cur = (cur, IntMap.update (Just . (+1)) (cur-prev) map)
    initialMap = IntMap.fromList [(1,0),(2,0),(3,0)]
    setWithPhone = IntSet.insert (IntSet.findMax adapterSet + 3) adapterSet
    
    
------------ PART B ------------
partB :: Input -> OutputB
partB adapterSet = IntMap.lookup 0 $ runDP (maxElem-1) dpMap
  where
   runDP :: Int -> IntMap Int -> IntMap Int
   runDP cur m 
     | cur >= 0 = runDP (cur-1) $ IntMap.alter update cur m
     | otherwise = m
        where
          update = fmap $ const $ IntMap.findWithDefault 0 (cur+3) m +
                                  IntMap.findWithDefault 0 (cur+2) m +
                                  IntMap.findWithDefault 0 (cur+1) m 

   dpMap :: IntMap Int
   dpMap = IntMap.fromSet (const 1) withEnds

   withEnds = IntSet.insert 0 $ IntSet.insert maxElem adapterSet
   maxElem = IntSet.findMax adapterSet + 3
