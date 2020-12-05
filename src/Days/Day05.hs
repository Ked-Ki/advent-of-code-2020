module Days.Day05 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Maybe

import Control.Applicative (Alternative((<|>)))

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text as P
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many' $ do
  row <- count 7 $ char 'F' <|> char 'B'
  col <- count 3 $ char 'L' <|> char 'R'
  endOfLine
  return $ SeatCode row col


------------ TYPES ------------
type Input = [SeatCode]

data SeatCode = SeatCode { rowStr :: String
                         , colStr :: String
                         }
                         deriving Show

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA codes = maximum $ map (rowColToSeatId . codeToRowCol) codes

-- seat id is just arbitrarily defined in the problem
rowColToSeatId :: (Int, Int) -> Int
rowColToSeatId (r,c) = r * 8 + c

codeToRowCol :: SeatCode -> (Int, Int)
codeToRowCol SeatCode{..} = ( strToInt rowCharToBit rowStr
                            , strToInt colCharToBit colStr
                            )
  where
    -- seat codes are "binary space partitioned", where F/L means first half, B/R means second half.
    -- Turns out this is identical to just using binary, since the number of rows & columns are both
    -- powers of two
    -- this fold just converts from "binary" to an Int by shifting the accumulated value left and
    -- adding the new digit
    strToInt :: (Char -> Int) -> String -> Int
    strToInt f = foldl (\acc c -> acc * 2 + f c) 0

    rowCharToBit 'F' = 0
    rowCharToBit 'B' = 1

    colCharToBit 'L' = 0
    colCharToBit 'R' = 1


------------ PART B ------------
partB :: Input -> OutputB
partB codes = fromJust $ findId sortedIds
  where
    -- look for two codes that are exactly 2 apart. result will be the number in between
    findId :: [Int] -> Maybe Int
    findId (x:xs@(y:_))
      | abs (x - y) == 2 = Just $ (x + y) `div` 2
      | otherwise        = findId xs
    findId [] = Nothing

    sortedIds = sort $ map (rowColToSeatId . codeToRowCol) codes
