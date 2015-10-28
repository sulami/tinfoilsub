module Main where

import           Control.Monad (unless)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)

import           Data.ConfigParser
import           Web.Server

main :: IO ()
main = do
  args <- getArgs
  unless (length args == 1) $ do
    putStrLn "Usage: tinfoilsub <config>"
    exitFailure
  feeds <- parseConfig <$> readFile (head args)
  runServer feeds

