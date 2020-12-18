module Days.Day16 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Maybe
import Data.List.PointedList.Circular (PointedList)
import qualified Data.List.PointedList.Circular as PL

import Data.Either (isRight, rights, lefts)
import Data.Bifunctor (Bifunctor(first))

import qualified Data.Text as T

import qualified Program.RunDay as R
import Data.Attoparsec.Text
import Control.Applicative.Combinators (many)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  rules <- sepBy ruleParser endOfLine <* endOfLine
  endOfLine

  "your ticket:" <* endOfLine
  yourTicket <- ticketParser
  endOfLine

  "nearby tickets:" <* endOfLine
  nearbyTickets <- many ticketParser

  return $ Input{..}

  where
    ruleParser :: Parser Rule
    ruleParser = do
      name <- takeTill (==':')
      ranges <- (,) <$> (": " *> rangeParser) <*> (" or " *> rangeParser)
      return $ Rule{..}

    rangeParser :: Parser Range
    rangeParser = (,) <$> (decimal <* "-") <*> decimal
    ticketParser :: Parser Ticket
    ticketParser = sepBy decimal "," <* endOfLine


------------ TYPES ------------
data Input = Input { rules :: [Rule]
                   , yourTicket :: Ticket
                   , nearbyTickets :: [Ticket]
                   }
             deriving Show

data Rule = Rule { name :: T.Text
                 , ranges :: (Range,Range)
                 }
            deriving (Show, Eq)

type Range = (Int,Int)

type Ticket = [Int]

type OutputA = Int
partA :: R.Part Input OutputA
partA = R.defaultPart "Part A" solveA

type OutputB = [(Rule, Int)]
partB :: R.Part Input OutputB
partB = R.Part { name = "Part B"
               , solve = solveB
               , showSol = unlines . map showField
               , toInt = toInteger . product . map snd . getDepartureFields
               }
        where
          getDepartureFields = filter (("departure" `T.isPrefixOf`) . name . fst)
          showField (Rule{..},x) = T.unpack name ++ ": " ++ show x


------------ PART A ------------
solveA :: Input -> OutputA
solveA Input{..} = sum $ concat $ lefts $ map (checkTicket rules) nearbyTickets

checkTicket :: [Rule] -> Ticket -> Either [Int] Ticket
checkTicket rs t = case invalidFields of
                     [] -> Right t
                     xs -> Left xs
  where
    valid x = any (matchRule x) rs
    invalidFields = filter (not . valid) t

matchRule :: Int -> Rule -> Bool
matchRule i Rule{ranges=(r1,r2)} = matchRange r1 || matchRange r2
  where
    matchRange (l,h) = l <= i && i <= h

------------ PART B ------------
solveB :: Input -> OutputB
solveB Input{..} = labeledTicket
  where
    labeledTicket = zip solvedRules yourTicket
    solvedRules = solveRules $ map (findRules rules) (transpose validTickets)
    validTickets = rights $ map (checkTicket rules) nearbyTickets

-- find the rules that are valid for all values for a given field
findRules :: [Rule] -> [Int] -> [Rule]
findRules rs xs = filter (\r -> all (`matchRule` r) xs) rs

-- left is a list of possible rules, right indicates the rule is solved
type SolvedRule = Either [Rule] Rule

-- figure out which of several possible rules is the correct one for each field.
--
-- We do this by iterating through the list possible rules for each field, fixing every field for
-- which there's only one rule, and eliminating that rule from the options for other fields.
--
-- note that this very much relies on there being a valid solution, it will loop forever if not.
solveRules :: [[Rule]] -> [Rule]
solveRules rss = go $ fromJust $ PL.fromList $ map Left rss
  where
    go :: PointedList SolvedRule -> [Rule]
    go rs = let updated = update rs
             in if all isRight updated
                   then rights $ plToList updated
                   else go $ PL.next updated

    plToList :: PointedList a -> [a]
    plToList = foldr (:) []

    -- update list iff the current focused SolvedRule is solved
    update :: PointedList SolvedRule -> PointedList SolvedRule
    update rs = case PL._focus rs of
                  Right _ -> rs -- already solved
                  Left curRules ->
                    if | null curRules -> error "found an empty rule list during solving"
                       -- if there's only one valid rule for this field, mark it solved & remove the
                       -- rule from all other rule lists
                       | length curRules == 1 -> let solvedR = head curRules
                            in first (filter (/=solvedR)) <$> PL.replace (Right solvedR) rs
                       | otherwise -> rs
