module Program.RunDay (runDay, Part(..), defaultPart) where

import Control.Exception (SomeException, catch)
import Control.Monad.Except
import Data.Attoparsec.Text
import Data.Text (pack)
import System.Directory (doesFileExist)

data Part i a = Part { name :: String
                     , solve :: i -> a
                     , showSol :: a -> String
                     , toInt :: a -> Integer
                     }

defaultPart :: (Show a, Integral a) => String -> (i -> a) -> Part i a
defaultPart name solve = Part{..}
  where
    showSol = show
    toInt = toInteger

runDay :: (Show i) => Parser i -> Part i a -> Part i b -> Bool -> String -> IO ()
runDay inputParser partA partB verbose inputFile = do
  input <- runExceptT $ do
    inputFileExists <- liftIO $ doesFileExist inputFile
    fileContents <-
      if inputFileExists
        then liftIO $ readFile inputFile
        else throwError $ "I couldn't read the input! I was expecting it to be at " ++ inputFile
    case parseOnly inputParser . pack $ fileContents of
      Left e -> throwError $ "Parser failed to read input. Error " ++ e
      Right i -> do
        when verbose $ do
          liftIO $ putStrLn "Parser output:"
          liftIO . print $ i
        return i
  processInput input
  where
    runPart :: i -> Part i a' -> IO ()
    runPart i Part{..} = putStrLn (name ++ ":") >>
      catch (let sol = solve i
              in when verbose (putStrLn $ showSol sol) >> 
                 print (toInt sol))
            (\m -> putStrLn ("Couldn't run " ++ name) >>
              when verbose (print (m :: SomeException))) 

    processInput (Left x) = putStrLn x
    processInput (Right i) = do
      runPart i partA
      runPart i partB
