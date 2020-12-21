module Days.Day18 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List

import qualified Program.RunDay as R
import Data.Attoparsec.Text as AP
import Control.Applicative (Alternative((<|>)))
import qualified Data.Text as T
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = T.lines . T.filter (not . isHorizontalSpace) <$> takeText

------------ TYPES ------------
type Input = [T.Text]

data Expr = ExVal Int
          | ExPlus Expr Expr 
          | ExMult Expr Expr
          | ExParen Expr
     deriving Show

type OutputA = [Expr]
partA :: R.Part Input OutputA
partA = R.Part { name = "Part A"
               , solve = solveA
               , showSol = intercalate "\n" . map ppExpr
               , toInt = toInteger . sum . map computeExpr
               }

  where
    ppExpr e = show e ++ "\n\t=" ++ show (computeExpr e)

type OutputB = [Expr]
partB :: R.Part Input OutputB
partB = partA { R.name = "Part B", R.solve = solveB }

------------ PART A ------------
solveA :: Input -> OutputA
solveA = map (parseLine (exParser Nothing))
  where
    exParser :: Maybe Expr -> Parser Expr
    exParser Nothing = unaryParser >>= \e -> exParser (Just e)
    exParser (Just e) = (binaryParser e >>= \e' -> exParser (Just e')) <|> return e

    unaryParser, exValParser, exParenParser :: Parser Expr
    unaryParser = exValParser <|> exParenParser
    exValParser  = ExVal <$> decimal
    exParenParser = ExParen <$> ("(" *> exParser Nothing <* ")")

    binaryParser, exPlusParser, exMultParser  :: Expr -> Parser Expr
    binaryParser e = exPlusParser e <|> exMultParser e
    exPlusParser = opParser '+' ExPlus
    exMultParser = opParser '*' ExMult

    opParser :: Char -> (Expr -> Expr -> Expr) -> Expr -> Parser Expr
    opParser c ctr prev = ctr prev <$> (char c *> unaryParser)

computeExpr :: Expr -> Int
computeExpr (ExVal x) = x
computeExpr (ExPlus x y) = computeExpr x + computeExpr y
computeExpr (ExMult x y) = computeExpr x * computeExpr y
computeExpr (ExParen x) = computeExpr x

parseLine :: Parser Expr -> T.Text -> Expr
parseLine p t = case parseOnly p t of
                  Right e -> e
                  Left s -> error $ "parse error: " ++ s


------------ PART B ------------
solveB :: Input -> OutputB
solveB = map (parseLine (exParser Nothing))
  where
    exParser :: Maybe Expr -> Parser Expr
    exParser Nothing = unaryParser >>= \e -> exParser (Just e)
    exParser (Just e) = (binaryParser e >>= \e' -> exParser (Just e')) <|> return e

    unaryParser, exValParser, exParenParser :: Parser Expr
    unaryParser = exValParser <|> exParenParser
    exValParser  = ExVal <$> decimal
    exParenParser = ExParen <$> ("(" *> exParser Nothing <* ")")

    binaryParser, exPlusParser, exMultParser  :: Expr -> Parser Expr
    binaryParser e = exPlusParser e <|> exMultParser e
    exPlusParser prev = ExPlus prev <$> (char '+' *> unaryParser)
    exMultParser prev = ExMult prev <$> (char '*' *> exParser Nothing)  
