module Days.Day19 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

import Control.Monad.State.Lazy (runState, get, put, State)

import Control.Applicative (Alternative((<|>)))

import qualified Program.RunDay as R
import Data.Attoparsec.Text as AP
import qualified Data.Text as T
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  assocRules <- sepBy ruleAssocParser endOfLine
  let rules = Map.fromList assocRules
  AP.takeWhile isEndOfLine
  messages <- sepBy messageParser endOfLine
  return $ Input {..}
    where
      ruleAssocParser :: Parser (Int,Rule)
      ruleAssocParser = (,) <$> (decimal <* char ':' <* skipHSpace) <*> ruleParser Nothing

      ruleParser :: Maybe Rule -> Parser Rule
      ruleParser Nothing = (charRuleParser <|> subRuleParser) >>= ruleParser . Just
      ruleParser (Just r) = orRuleParser r <|> return r

      charRuleParser, subRuleParser :: Parser Rule
      charRuleParser = CharRule <$> (char '"' *> anyChar <* char '"')
      subRuleParser = SubRules <$> sepBy decimal skipHSpace

      orRuleParser :: Rule -> Parser Rule
      orRuleParser prevR = OrRule prevR <$> (skipHSpace *> char '|' *> skipHSpace *>
                                             ruleParser Nothing)

      messageParser :: Parser Message
      messageParser = T.unpack <$> AP.takeWhile1 (not . isEndOfLine)

      skipHSpace = AP.takeWhile isHorizontalSpace


------------ TYPES ------------
data Input = Input { rules :: Rules
                   , messages :: [Message]
                   }
                   deriving Show

type Rules = Map Int Rule

data Rule = CharRule Char
          | SubRules [Int]
          | OrRule Rule Rule
          deriving Show

type Message = String

type OutputA = [Message]
partA :: R.Part Input OutputA
partA = R.Part { name = "Part A"
               , solve = solveA
               , showSol = unlines
               , toInt = toInteger . length
               }

type OutputB = [Message]
partB :: R.Part Input OutputB
partB = partA { R.name = "Part B"
              , R.solve = solveB
              }

------------ PART A ------------
solveA :: Input -> OutputA
solveA Input{..} = filter (runMatch (rules!0)) messages
  where
    runMatch :: Rule -> Message -> Bool
    runMatch r m = case runState (checkMatch r) m of
                     (res,[]) -> res
                     (_  ,_ ) -> False

    checkMatch :: Rule -> State Message Bool
    checkMatch curRule = case curRule of
        CharRule k -> get >>= \case
                        (c:cs) -> if k == c
                                        then put cs >> return True
                                        else return False
                        [] -> return False
        SubRules rns -> and <$> mapM (checkMatch . (rules!)) rns

        OrRule r1 r2 -> branch [checkMatch r1, checkMatch r2]

    branch :: [State s Bool] -> State s Bool
    branch (s1:ss) = do
      startState <- get
      result1 <- s1
      if result1
          -- current branch succeeds, return
          then return True
          -- rewind state and try next branch
          else put startState >> branch ss
    -- out of branches, return false (state was already rewound in other pattern match)
    branch [] = return False


------------ PART B ------------
solveB :: Input -> OutputB
solveB = error "not implemented"
