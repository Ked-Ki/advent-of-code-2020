module Util.Parsers (coordinateParser, Coordinates(..)) where

import Data.Attoparsec.Text
import Data.Map (Map)
import qualified Data.Map as Map

{-
This module contains a list of parsers which are likely to be useful for Advent of Code problems.
-}

data Coordinates a = Coordinates { coord_map :: Map (Int, Int) a
                                 , x_bound :: Int
                                 , y_bound :: Int
                                 }
                     deriving Show

-- Takes a "mapper" "function that might map a char to a datatype, and an initial index (usually 0 or 1)
-- Returns a parser that returns a map from coordinates to all instances where the function returns Just
coordinateParser :: (Char -> Maybe a) -> Int -> Parser (Coordinates a)
coordinateParser mapper start = coordinateParser' start start
  where
    coordinateParser' x y =
      choice
        -- First we look for a line break, and we reset the coordinates appropriately
        [ endOfLine >> coordinateParser' start (y + 1),
          -- Then we look for a character, and map it
          anyChar >>= (\c -> addToMap mapper x y c <$> coordinateParser' (x + 1) y),
          -- Catches the EOF
          return $ Coordinates Map.empty start start
        ]
    addToMap mapper x y c Coordinates{..} = 
      Coordinates { coord_map = Map.alter (const (mapper c)) (x, y) coord_map
                  , x_bound = if x > x_bound then x else x_bound
                  , y_bound = if y > y_bound then y else y_bound
                  }
