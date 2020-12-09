{-# LANGUAGE TemplateHaskell #-}

module Days.Day08 (runDay, exState) where

{- ORMOLU_DISABLE -}
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array

import Data.Functor (($>))

import Control.Monad.State
import Control.Lens

import Data.Attoparsec.Text

import qualified Program.RunDay as R (runDay)
{- ORMOLU_ENABLE -}

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

data SystemState = SystemState { _acc :: Int
                               , _pc :: Int
                               }
                               deriving Show
makeLenses ''SystemState

data ExecutionState = ExecutionState { _sysState :: SystemState
                                     , _visited :: Set Int
                                     }
                                     deriving Show
makeLenses ''ExecutionState

data BacktrackState = BacktrackState { _exState :: ExecutionState
                                     , _modified :: Bool
                                     }
                                     deriving Show

makeLenses ''BacktrackState

type OutputA = SystemState

type OutputB = SystemState

------------ PART A ------------
partA :: Input -> OutputA
partA ipt = view sysState $ execState (runTillLoop ipt) initState
  where
    initState = ExecutionState (SystemState 0 0) Set.empty

runTillLoop :: Input -> State ExecutionState ()
runTillLoop program = go
  where
    go = do
      curPC <- use (sysState.pc)
      curVisited <- use visited
      if Set.member curPC curVisited
         then return () -- hit a loop, bail out
         else let inst = program ! curPC
              in visited %= Set.insert curPC >>
                 runInst inst >>
                 go

    runInst inst = case inst of
             Nop _ -> sysState.pc += 1
             Acc i -> sysState.acc += i >> sysState.pc += 1
             Jmp i -> sysState.pc += i


------------ PART B ------------
partB :: Input -> OutputB
partB ipt = view (exState.sysState) $ execState (backtrackOnLoop ipt) initState
  where
    initState = BacktrackState (ExecutionState (SystemState 0 0) Set.empty) False

backtrackOnLoop :: Input -> State BacktrackState ()
backtrackOnLoop program = go $> ()
  where
    go :: State BacktrackState Bool
    go = do
      curPC <- use $ exState.sysState.pc
      curVisited <- use $ exState.visited
      if | Set.member curPC curVisited -> return False -- hit a loop, fail cur branch
         | curPC > snd (bounds program) -> return True -- made it to the end of program, success
         | otherwise -> do -- run next instruction
             unwindState <- get
             (exState.visited) %= Set.insert curPC
             let inst = program ! curPC
             case inst of
               Acc i -> runAcc i >> go
               Jmp i -> runJmp i >> go >>= unwindOrGo unwindState runNop 
               Nop i -> runNop >> go >>= unwindOrGo unwindState (runJmp i)

    runNop :: State BacktrackState ()
    runNop = (exState.sysState.pc) += 1

    runAcc, runJmp :: Int -> State BacktrackState ()
    runAcc i = (exState.sysState.pc) += 1 >> (exState.sysState.acc) += i
    runJmp i = (exState.sysState.pc) += i

    unwindOrGo :: BacktrackState -> State BacktrackState () -> Bool -> State BacktrackState Bool
    unwindOrGo _ _ True = return True -- previous instruction led to termination, just return
    unwindOrGo unwoundState runModifiedInst False = 
      put unwoundState >> use modified >>= \isModified ->
        if isModified
           -- we've already modified one instruction, this branch is dead
           then return False
           -- otherwise, modify current instruction and continue running
           else modified .= True >> runModifiedInst >> go

-- Run day (moved to bottom to accommodate TH)
runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

