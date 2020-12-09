module Days.Day09 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Control.Applicative.Combinators (many)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many $ decimal <* endOfLine

------------ TYPES ------------
type Input = [Int]

contextLen :: Int
contextLen = 25

-- FIFO queue plus a set, for fast lookups. 
-- invariant: contents == Set.fromList _head++_tail
data QueueSet a = QueueSet { _head :: [a]
                           , _tail :: [a]
                           , contents :: Set a
                           }
                           deriving Show

instance Foldable QueueSet where
  -- NB: this foldable instance doesn't preserve queue order.
  -- this is good enough for today's problem, but make sure to fix it if you reuse this
  foldMap f QueueSet{..} = foldMap f contents

member :: (Ord a) => a -> QueueSet a -> Bool
member x QueueSet{..} = Set.member x contents

push :: (Ord a) => a -> QueueSet a -> QueueSet a
push x q@QueueSet{..} = q{_tail=x:_tail, contents=newSet}
  where
    newSet = Set.insert x contents

pop :: (Ord a) => QueueSet a -> QueueSet a
pop q@QueueSet{..} =
  case _head of
    (y:ys) -> q {_head=ys, contents=mkNewSet y}
    [] -> let y:newHead = reverse _tail
          in q {_head = newHead, _tail=[], contents=mkNewSet y}
  where
    mkNewSet y = Set.delete y contents

shiftQueue :: (Ord a) => a -> QueueSet a -> QueueSet a
shiftQueue x = pop . push x

fromList :: (Ord a) => [a] -> QueueSet a
fromList xs = QueueSet{_head=xs, _tail=[], contents=Set.fromList xs}

type OutputA = Maybe Int

type OutputB = Int

------------ PART A ------------

-- find the first number in the input that is NOT the sum of two numbers in the preceding
-- `contextLen` numbers
partA :: Input -> OutputA
partA ipt = go rest $ fromList preamble
  where
    (preamble,rest) = splitAt contextLen ipt
    go :: [Int] -> QueueSet Int -> Maybe Int
    go (x:xs) q
      | any (\a -> member (x-a) q) q = go xs (shiftQueue x q)
      | otherwise = Just x
    go [] _ = Nothing

------------ PART B ------------

-- find a contiguous set of numbers in the input that sum to partA, return min + max of that set
partB :: Input -> OutputB
partB ipt = Set.findMin result + Set.findMax result
  where
    target = fromJust $ partA ipt
    result = contents $ go ipt $ fromList []
    go :: [Int] -> QueueSet Int -> QueueSet Int
    go l@(x:xs) q = case compare (sum q) target of
                      GT -> go l (pop q)
                      EQ -> q
                      LT -> go xs (push x q)
    go [] _ = fromList []
