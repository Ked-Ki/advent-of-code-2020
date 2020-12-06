module Days.Day06 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Foldable (Foldable(fold))
import qualified Data.Text as T
import Data.Char (isLower)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy (many' personParser) endOfLine
  where
    personParser :: Parser (Set Char)
    personParser = Set.fromList . T.unpack <$> takeWhile1 isLower <* endOfLine

------------ TYPES ------------
type Input = [[Set Char]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . map (Set.size . fold)

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map (Set.size . foldr Set.intersection completeSet)
  where
    completeSet = Set.fromList "abcdefghijklmnopqrstuvwxyz"
