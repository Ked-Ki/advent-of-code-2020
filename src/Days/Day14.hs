module Days.Day14 (runDay) where

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

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Data.Word (Word64)
import Control.Applicative (Alternative((<|>)))
import Data.Functor (($>))
import Data.Bits
import Control.Applicative.Combinators (many)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many instParser
  where
    instParser :: Parser Inst
    instParser = (maskParser <|> setParser) <* endOfLine
    maskParser :: Parser Inst
    maskParser = MaskI <$> ("mask = " *> many (choice ["X" $> X, "0" $> Zero, "1" $> One]))
    setParser :: Parser Inst
    setParser = Set <$> ("mem[" *> decimal) <*> ("] = " *> decimal)

------------ TYPES ------------
type Input = [Inst]

data Inst = MaskI [MBit]
          | Set  { addr :: Word64
                 , val :: Word64
                 }
          deriving Show

data MBit = One | Zero | X
          deriving Show


type Memory = Map Word64 Word64

initMem :: Memory
initMem = Map.empty

type OutputA = Word64

type OutputB = Word64

------------ PART A ------------
partA :: Input -> OutputA
partA = sumMemory . go initMem initMaskA
  where
    go :: Memory -> MaskA -> [Inst] -> Memory
    go mem msk (Set{..}       :is') = let newMem = Map.insert addr (applyMaskA msk val) mem
                                       in go newMem msk is'
    go mem _   ((MaskI newMsk):is') = go mem (toMaskA newMsk) is'
    go mem _   []                   = mem

sumMemory :: Memory -> Word64
sumMemory mem = sum $ Map.elems mem

data MaskA = MaskA { oneMask  :: Word64 -- when or'd, sets all 1s correctly
                   , zeroMask :: Word64 -- when and'd, sets all 0s correctly
                   }
            deriving Show

toMaskA :: [MBit] -> MaskA
toMaskA = go 35 zeroBits (complement zeroBits)
  where
    go :: Int -> Word64 -> Word64 -> [MBit] -> MaskA
    go (-1) oneAcc zeroAcc []       = MaskA oneAcc zeroAcc
    go i oneAcc zeroAcc (X:mbs')    = go (i-1) (clearBit oneAcc i) (setBit zeroAcc i) mbs'
    go i oneAcc zeroAcc (Zero:mbs') = go (i-1) (clearBit oneAcc i) (clearBit zeroAcc i) mbs'
    go i oneAcc zeroAcc (One:mbs')  = go (i-1) (setBit oneAcc i)   (setBit zeroAcc i) mbs'

initMaskA :: MaskA
initMaskA = MaskA zeroBits (complement zeroBits)

applyMaskA :: MaskA -> Word64 -> Word64
applyMaskA MaskA{..} x = (x .|. oneMask) .&. zeroMask


------------ PART B ------------
partB :: Input -> OutputB
partB = sumMemory . go initMem initMaskB
  where
    go :: Memory -> MaskB -> [Inst] -> Memory
    go mem msk (Set{..}       :is') = let newMem = foldr (`Map.insert` val) mem (applyMaskB msk addr)
                                       in go newMem msk is'
    go mem _   ((MaskI newMsk):is') = go mem newMsk is'
    go mem _   []                   = mem

type MaskB = [MBit]

initMaskB :: MaskB
initMaskB = replicate 36 Zero

applyMaskB :: MaskB -> Word64 -> [Word64]
applyMaskB = go 35
  where
    go :: Int -> [MBit] -> Word64 -> [Word64]
    go (-1) [] curX = [curX]
    go i (Zero:mbs') curX = go (i-1) mbs' curX
    go i (One:mbs') curX = go (i-1) mbs' (setBit curX i)
    go i (X:mbs') curX = go (i-1) mbs' (setBit curX i) ++ go (i-1) mbs' (clearBit curX i)
