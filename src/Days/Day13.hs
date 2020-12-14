module Days.Day13 (runDay) where

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

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Control.Applicative.Combinators ((<|>))
import Data.Ord (Down(..), comparing)
import Data.Bifunctor (Bifunctor(second))
import Debug.Trace (traceShow)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  departTime <- decimal <* endOfLine
  ids <- sepBy ((Just <$> decimal) <|> (Nothing <$ "x")) ","
  return (departTime,ids)


------------ TYPES ------------
type Input = (Integer, [Maybe Integer])

type OutputA = (Integer, Integer)

type OutputB = Integer

------------ PART A ------------
partA :: Input -> OutputA
partA (dTime, ids) = second negate busIdWaitTime
  where
    busIdWaitTime = maximumBy (comparing snd) idsAndWaitTimes
    idsAndWaitTimes = map ((,) <$> id <*> timeToNext) $ catMaybes ids
    timeToNext id = (dTime `mod` id) - id

------------ PART B ------------
partB :: Input -> OutputB
partB (_,bs) = go 0 allMbs
  where
    -- turns out that a "pattern" of bus departures at time x for some bus ids [b1,b2..bn] next
    -- reoccurs at exactly x + product(b1,b2..bn). While this was determined experimentally, it
    -- makes sense since a) the input is all prime numbers and b) a pattern will reoccur for sure
    -- at x + lcm(b1,b2..bn).
    --
    -- This takes advantage of that fact and skips ahead by the product of all bus ids that matched
    -- the desired "pattern" of departures on each loop.
    go x mbs = let stride = product $ map snd $ filter (kiExists x) mbs
                in if all (kiExists x) mbs
                      then x
                      else go (x + stride) mbs

    kiExists :: Integer -> (Integer,Integer) -> Bool
    kiExists x (mi,bi) = (x + mi) `mod` bi == 0

    allMbs :: [(Integer,Integer)]
    allMbs = map (second fromJust) $ filter (isJust . snd) $ zip [0..] bs

{-
bruteForceB :: Input -> OutputB
bruteForceB (_,ids) = go possibleStartTimes
  where
    go :: [[Integer]] -> Integer
    go ((t:ts):tss) = if all (findTimeInSorted t) tss
                         then t
                         else go (ts: map (dropWhile (<t)) tss)
    possibleStartTimes :: [[Integer]]
    possibleStartTimes = map allValidStartTimes modsAndIds
    findTimeInSorted :: Integer -> [Integer] -> Bool
    findTimeInSorted t (t':ts) 
      | t < t' = False
      | t == t' = True
      | otherwise = findTimeInSorted t ts
    allValidStartTimes :: (Integer,Integer) -> [Integer]
    allValidStartTimes (m,id) = subtract m <$> [id,id*2..]
    modsAndIds :: [(Integer,Integer)]
    modsAndIds = sortOn (Down . snd) $ map (second fromJust) $ filter (isJust . snd) $ zip [0..] ids
-}
