module Days.Day13 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Maybe

import qualified Program.RunDay as R
import Data.Attoparsec.Text
import Control.Applicative.Combinators ((<|>))
import Data.Ord (comparing)
import Data.Bifunctor (Bifunctor(second))
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
partA :: R.Part Input OutputA
partA = R.Part { name = "Part A"
               , solve = solveA
               , showSol = show
               , toInt = uncurry (*)
               }

type OutputB = Integer
partB :: R.Part Input OutputB
partB = R.defaultPart "Part B" solveB

------------ PART A ------------
solveA :: Input -> OutputA
solveA (dTime, ids) = second negate busIdWaitTime
  where
    busIdWaitTime = maximumBy (comparing snd) idsAndWaitTimes
    idsAndWaitTimes = map ((,) <$> id <*> timeToNext) $ catMaybes ids
    timeToNext id = (dTime `mod` id) - id

------------ PART B ------------
solveB :: Input -> OutputB
solveB (_,bs) = go 0 allMbs
  where
    -- Let `b1,b2..bn` be the busses in the input, and let `m1,m2..mn` be the offset at which they
    -- appear in the list. 
    --
    -- i.e. input of 17,x,13,19 has b1=17,m1=0; b2=13,m2=2; b3=19,m3=3
    --
    -- The goal of this part is to find a time `x` at which there exist `k1,k2..kn` s.t. 
    -- `x + mi = ki * bi` for all `mi`,`bi`
    --
    -- Theorem: 
    -- If there exists some `x` at which `x + mj = kj * bj` for all `j` in some subset `J`
    -- of [1..n], then the next such `x` & `kj`s occur at `x + lcm(bj | j <- J)`. 
    -- (i.e. if some time works for a couple busses, the next time those busses will work is lcm of
    -- those busses away).
    --
    -- Proof:
    -- Let `B = bj | j <- J`. at some time `y < x`, all busses in B depart simultaneously. 
    -- `x - y` minutes later, busses `bj | j <- J` depart in the correct order. 
    -- The next time all `b <- B` depart simultaneously is at `y + lcm(B)`, and thus the busses will
    -- depart in the correct order `x - y` minutes after that, at `x + lcm(B)`. Moreover, there is
    -- no other time `x'` where `x < x' < x + lcm(B)` at which this is true, because then at time
    -- `x' - (x - y)` all busses will depart simultaneously, which would mean `x' - (x - y)` is a
    -- multiple of all `bj <- B`, which contradicts the definition of least-common-multiple. QED
    --
    -- Thus, whenever we find an `x` that works for some set of busses, we can immediately skip
    -- ahead by the lcm of that set of busses, and this will always find the smallest timestamp.
    go x mbs = let stride = lcmList $ map snd $ filter (kiExists x) mbs
                in if all (kiExists x) mbs
                      then x
                      else go (x + stride) mbs

    lcmList :: [Integer] -> Integer
    lcmList = foldr1 lcm

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
