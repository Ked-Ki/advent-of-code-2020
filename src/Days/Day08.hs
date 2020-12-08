module Days.Day08 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array

import Data.Bifunctor (first, second)
import Data.Functor (($>))

import Control.Monad.State

import Data.Attoparsec.Text

import qualified Program.RunDay as R (runDay)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  instrs <- instructionList
  return $ listArray (0, length instrs - 1) instrs
  where
    instructionList = (instructionParser <*> signed decimal) `sepBy` endOfLine

    instructionParser :: Parser (Int -> Instruction)
    instructionParser = choice [ "nop" $> Nop
                               , "jmp" $> Jmp
                               , "acc" $> Acc
                               ] <* space

------------ TYPES ------------
type Input = Array Int Instruction

data Instruction = Nop Int
                 | Jmp Int
                 | Acc Int
                 deriving Show

data SystemState = SystemState { acc :: Int
                               , pc :: Int
                               }
                               deriving Show

-- TODO use lens
overAcc, overPC :: (Int -> Int) -> SystemState -> SystemState
overAcc f ss = ss { acc = f (acc ss) }
overPC f ss = ss { pc = f (pc ss) }

type OutputA = SystemState

type OutputB = SystemState

------------ PART A ------------
partA :: Input -> OutputA
partA ipt = fst $ execState (runTillLoop ipt) initState
  where
    initState = (SystemState 0 0, Set.empty)

runTillLoop :: Input -> State (SystemState, Set Int) ()
runTillLoop program = go
  where
    go = do
      curPC <- gets (pc . fst)
      visited <- gets snd
      if Set.member curPC visited
         then return () -- hit a loop, bail out
         else let inst = program ! curPC
              in modify (second $ Set.insert curPC) >>
                 runInst inst >>
                 go

    runInst inst = case inst of
             Nop _ -> modify (first $ overPC (+1))
             Acc i -> modify (first $ overAcc (+i) . overPC (+1))
             Jmp i -> modify (first $ overPC (+i))


------------ PART B ------------
partB :: Input -> OutputB
partB ipt = sysState $ execState (backtrackOnLoop ipt) initState
  where
    initState = BacktrackState (SystemState 0 0) Set.empty False

-- TODO make TillLoopState w/o modified & have both implement overSysState & overVisited
-- this will allow sharing run* methods b/w both parts
-- ??? will this work with lenses w/o a typeclass?

data BacktrackState = BacktrackState { sysState :: SystemState
                                     , visited :: Set Int
                                     , modified :: Bool
                                     }
                                     deriving Show

overVisited :: (Set Int -> Set Int) -> BacktrackState -> BacktrackState
overVisited f bs = bs { visited = f (visited bs) }

overSysState :: (SystemState -> SystemState) -> BacktrackState -> BacktrackState
overSysState f bs = bs { sysState = f (sysState bs) }

setModified :: BacktrackState -> BacktrackState
setModified bs = bs { modified = True }

backtrackOnLoop :: Input -> State BacktrackState ()
backtrackOnLoop program = go $> ()
  where
    go :: State BacktrackState Bool
    go = do
      curPC <- gets (pc . sysState)
      visited <- gets visited
      if | Set.member curPC visited -> return False -- hit a loop, fail cur branch
         | curPC > snd (bounds program) -> return True -- made it to the end of program, success
         | otherwise -> do -- run next instruction
             unwindState <- get
             modify (overVisited (Set.insert curPC))
             let inst = program ! curPC
             case inst of
               Acc i -> runAcc i >> go
               Jmp i -> runJmp i >> go >>= unwindOrGo unwindState runNop 
               Nop i -> runNop >> go >>= unwindOrGo unwindState (runJmp i)

    runNop :: State BacktrackState ()
    runNop = modify (overSysState (overPC (+1)))

    runAcc, runJmp :: Int -> State BacktrackState ()
    runAcc i = modify (overSysState (overAcc (+i) . overPC (+1)))
    runJmp i = modify (overSysState (overPC (+i)))

    unwindOrGo :: BacktrackState -> State BacktrackState () -> Bool -> State BacktrackState Bool
    unwindOrGo _ _ True = return True -- previous instruction led to termination, just return
    unwindOrGo unwoundState runModifiedInst False = 
      put unwoundState >> gets modified >>= \isModified ->
        if isModified
           -- we've already modified one instruction, this branch is dead
           then return False
           -- otherwise, modify current instruction and continue running
           else modify setModified >> runModifiedInst >> go
