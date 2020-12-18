module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R
import Data.Attoparsec.Text
import qualified Data.Text as T
import Data.Char (isSpace)
import Data.Maybe (isJust)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy passportParser (char '\n')
  where
    passportParser :: Parser [(T.Text,T.Text)]
    passportParser = sepBy (choice (keyValParser . key <$> keys)) space 
      -- consume the trailing newline, otherwise we get empty passports b/w lines
      <* space

    keyValParser :: T.Text -> Parser (T.Text,T.Text)
    keyValParser k = (,) <$> string k <* char ':' <*> takeTill isSpace


------------ TYPES ------------
type Input = [[(T.Text, T.Text)]]

type OutputA = Int
partA :: R.Part Input OutputA
partA = R.defaultPart "Part A" solveA

type OutputB = Int
partB :: R.Part Input OutputB
partB = R.defaultPart "Part B" solveB

-- list of all passport fields
keys :: [FieldDef]
keys = [ FieldDef "byr" True  (yearValid 1920 2002)
       , FieldDef "iyr" True  (yearValid 2010 2020)
       , FieldDef "eyr" True  (yearValid 2020 2030)
       , FieldDef "hgt" True  (validateParser heightParser heightValid)
       , FieldDef "hcl" True  (checkParse hairColorParser)
       , FieldDef "ecl" True  (checkParse eyeColorParser)
       , FieldDef "pid" True  (checkParse passIdParser)
       , FieldDef "cid" False (const True)
       ]
  where
    yearValid :: Int -> Int -> T.Text -> Bool
    yearValid l h = validateParser (decimal :: Parser Int) (inBounds l h)

    heightParser :: Parser Height
    heightParser = choice [ Centimeter <$> (decimal :: Parser Int) <* string "cm"
                          , Inch <$> (decimal :: Parser Int) <* string "in"
                          ]

    heightValid :: Height -> Bool
    heightValid (Centimeter x) = inBounds 150 193 x
    heightValid (Inch x) = inBounds 59 76 x

    -- we don't care what the value is for the remaining fields, so we return () and just check that
    -- parseOnly returns a Right
    hairColorParser :: Parser ()
    hairColorParser = char '#' *> count 6 (satisfy (inClass "0-9a-f")) *> endOfInput

    eyeColorParser :: Parser ()
    eyeColorParser = choice (string <$> ["amb","blu","brn","gry","grn","hzl","oth"]) *> endOfInput

    passIdParser :: Parser ()
    passIdParser = count 9 digit *> endOfInput

    validateParser :: Parser a -> (a -> Bool) -> T.Text -> Bool
    validateParser parser pred t = either (const False) pred (parseOnly parser t)
    -- NOTE: we have to use parseOnly here since we want to check exact lengths. with `parse`,
    -- endOfInput won't parse

    checkParse :: Parser () -> T.Text -> Bool
    checkParse p = validateParser p (const True)

    inBounds l h x = l <= x && x <= h

-- field names & validation rules
data FieldDef = FieldDef { key :: T.Text
                         , required :: Bool
                         , validateVal :: T.Text -> Bool
                         }

data Height = Centimeter Int | Inch Int


------------ PART A ------------
solveA :: Input -> OutputA
solveA = length . filter isValid
  where
    isValid :: [(T.Text,T.Text)] -> Bool
    isValid kvs = all (\FieldDef{..} -> not required || isJust (lookup key kvs)) keys

------------ PART B ------------
solveB :: Input -> OutputB
solveB = length . filter isValid
  where
    isValid :: [(T.Text,T.Text)] -> Bool
    isValid kvs = all (\FieldDef{..} -> not required || maybe False validateVal (lookup key kvs)) keys
