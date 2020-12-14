{-# LANGUAGE NamedFieldPuns #-}

module Days.Day02 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Data.Text as T

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
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
partA = length . filter isValidA

isValidA :: Password -> Bool
isValidA Password{..} = min <= numMatches && numMatches <= max
  where
    numMatches = T.length $ T.filter (letter ==) password

------------ PART B ------------
partB :: Input -> OutputB
partB = length . filter isValidB

isValidB :: Password -> Bool
isValidB Password{..} = minMatch /= maxMatch
  where
    checkIdx :: Int -> T.Text -> Bool
    checkIdx i t = letter == T.index t (i-1)

    minMatch = checkIdx min password
    maxMatch = checkIdx max password
