module Util.Parsers (coordinateParser, Coordinates(..), prettyPrintCoords, overM) where

import Data.Attoparsec.Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor ((<&>))

{-
This module contains a list of parsers which are likely to be useful for Advent of Code problems.
-}

type Bounds = (Int,Int)

data Coordinates a = Coordinates { coord_map :: Map (Int, Int) a
                                 , x_bound :: Bounds
                                 , y_bound :: Bounds
                                 }
                     deriving Show

overM :: (Map (Int,Int) a -> Map (Int,Int) a) -> Coordinates a -> Coordinates a
overM f c@Coordinates{..} = c { coord_map = f coord_map }

prettyPrintCoords :: Coordinates a -> (Maybe a -> Char) -> String
prettyPrintCoords Coordinates{..} toC = uncurry enumFromTo y_bound >>= \y -> 
  (uncurry enumFromTo x_bound <&> \x -> toC $ Map.lookup (x,y) coord_map) ++ ['\n']


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
          return $ Coordinates Map.empty (start,start) (start,start)
        ]
    addToMap mapper x y c Coordinates{..} = 
      Coordinates { coord_map = Map.alter (const (mapper c)) (x, y) coord_map
                  , x_bound = if x > snd x_bound then (fst x_bound, x) else x_bound
                  , y_bound = if y > snd y_bound then (fst y_bound, y) else y_bound
                  }
