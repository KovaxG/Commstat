module Main where

import System.Environment
import Commit

main :: IO ()
main = do
  directory <- safeHead "." <$> getArgs
  putStrLn $ "Parsing Commits from " ++ directory ++ "..."
  commits <- readAllCommits directory
  putStrLn "Writing to CSV..."
  toCSV "data.csv" commits
  putStrLn "Done."

toCSV :: String -> [Commit] -> IO () 
toCSV path commits =
  writeFile path $ unlines $ map toLine commits
  where
    toLine c = date c ++ "," ++ show (linesChanged c)

safeHead :: a -> [a] -> a
safeHead a [] = a 
safeHead _ as = head as
