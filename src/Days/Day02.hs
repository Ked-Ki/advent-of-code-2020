{-# LANGUAGE NamedFieldPuns #-}

module Days.Day02 (runDay) where

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

import qualified Data.Text as T

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many' $ do
  min <- decimal
  char '-'
  max <- decimal
  skipSpace
  letter <- anyChar
  char ':'
  skipSpace
  password <- takeTill isEndOfLine 
  endOfLine
  return $ Password letter min max password

------------ TYPES ------------
type Input = [Password]

data Password = Password { letter :: Char
                         , min :: Int
                         , max :: Int
                         , password :: T.Text
                         }
                deriving Show

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = foldr ((+) . boolToInt . isValidA) 0

isValidA :: Password -> Bool
isValidA (Password {letter, min, max, password}) = min <= numMatches && numMatches <= max
  where
    numMatches = T.foldr ((+) . boolToInt . (letter ==)) 0 password

boolToInt :: Bool -> Int
boolToInt b
  | b = 1
  | otherwise = 0

------------ PART B ------------
partB :: Input -> OutputB
partB = foldr ((+) . boolToInt . isValidB) 0

isValidB :: Password -> Bool
isValidB (Password {letter, min, max, password}) = minMatch /= maxMatch
  where
    checkIdx :: Int -> T.Text -> Bool
    checkIdx i t = letter == T.index t (i-1)

    minMatch = checkIdx min password
    maxMatch = checkIdx max password
