module Days.Day07 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.State

import Control.Applicative (liftA2, (<|>))
import Data.Bifunctor (first)
import Data.Functor (($>))

import Data.Attoparsec.Text
import Control.Applicative.Combinators (many)
import qualified Data.Text as T
import Data.Char (isPunctuation)

import qualified Program.RunDay as R (runDay)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many $ do
    outerBag <- bagTypeParser
    "contain"
    space
    contents <- emptyBagParser <|> many (liftA2 (,) (decimal <* space) bagTypeParser)
    endOfLine
    return Rule { outer = outerBag
                , contents = contents
                }
  where
    bagTypeParser :: Parser BagType
    bagTypeParser = T.unwords <$> count 2 anyWord
                              <* ("bags" <|> "bag")
                              <* skipWhile shouldSkip

    emptyBagParser :: Parser [(Int, BagType)]
    emptyBagParser = "no other bags." $> []

    anyWord :: Parser T.Text
    anyWord = takeTill shouldSkip <* skipWhile shouldSkip

    shouldSkip = liftA2 (||) isHorizontalSpace isPunctuation

------------ TYPES ------------
type Input = [Rule]

type BagType = T.Text

data Rule = Rule { outer :: BagType
                 , contents :: [(Int, BagType)]
                 }
                 deriving Show

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA rules = length (bfsGraph (mkGraph rules) "shiny gold") - 1
  where
    -- make a graph from inner bag type to bag types that can hold it
    mkGraph :: [Rule] -> Map BagType [BagType]
    mkGraph (Rule{..}:rs) = foldr (Map.alter addType) (mkGraph rs) (snd <$> contents)
      where
        addType (Just ts) = Just (outer:ts)
        addType Nothing = Just [outer]
    mkGraph [] = Map.empty

bfsGraph :: forall a. (Ord a) => Map a [a] -> a -> [a]
bfsGraph m start = evalState go ([start], Set.empty)
  where
    go :: State ([a], Set a) [a]
    go = get >>= \case
        (curNode:rest, visited) ->
          if Set.member curNode visited
          then modify (first (const rest)) >> go
          else let newVisited = Set.insert curNode visited
                   successors = getSuccessors m newVisited curNode
               in  put (rest++successors, newVisited) >>
                   (curNode:) <$> go
        ([],_) -> return []


    getSuccessors :: Map a [a] -> Set a -> a -> [a]
    getSuccessors graph visited key = filter notVisited $ fromMaybe [] $ Map.lookup key graph
      where
        notVisited = not . flip Set.member visited


------------ PART B ------------
partB :: Input -> OutputB
partB rules = countBags (mkGraph rules) "shiny gold" - 1
  where
    mkGraph :: [Rule] -> Map BagType [(Int,BagType)]
    mkGraph = Map.fromList . map ((,) <$> outer <*> contents)

    countBags :: forall a. (Ord a) => Map a [(Int,a)] -> a -> Int
    countBags m start = go start
      where
        go :: a -> Int
        go curNode = foldr (\(c,node) acc -> acc + c * go node)
                           1
                           (fromMaybe [] $ Map.lookup curNode m)
