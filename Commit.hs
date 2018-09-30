module Commit (
  Commit (..),
  linesChanged,
  readAllCommits
) where

import Text.ParserCombinators.Parsec
import Data.List
import Data.Maybe
import System.Directory

data Commit = Commit {
    hash :: String,
    author :: String,
    date :: String,
    description :: String,
    filesTouched :: Int,
    linesAdded :: Int,
    linesRemoved :: Int
} deriving (Show, Eq)

linesChanged :: Commit -> Int
linesChanged commit = added - removed
  where
    added = linesAdded commit
    removed = linesRemoved commit

parseCommit :: String -> Either ParseError Commit
parseCommit = parse rule "Parsing Commit"
  where
    rule = do
      string "commit "
      gitHash <- many alphaNum
      newline
      _ <- many (noneOf "A" *> many (noneOf "\n") *> newline)
      string "Author: "
      authorName <- many (noneOf "\n")
      newline
      string "Date: "
      commitDate <- dateRule 
      newline
      commitMessage <- many (descriptionLine <|> string "\n")
      changes <- many changeLine
      return Commit {
        hash = gitHash,
        author = authorName,
        date = commitDate,
        description = commitMessage >>= (++ "\n"),
        filesTouched = length changes,
        linesAdded = sum $ map (read . fst) changes,
        linesRemoved = sum $ map (read . snd) changes
      }

    descriptionLine = do
      space
      content <- many (noneOf "\n")
      newline
      return content

    changeLine = do
      added <- many digit
      char '\t'
      removed <- many digit
      many (noneOf "\n")
      newline
      return (added, removed)
    
    dateRule = do
      spaces  
      many letter
      space
      month <- fromMaybe 0 . parseMonth <$> many letter
      space
      day <- many digit
      spaces
      many (noneOf " ")
      spaces
      year <- many digit
      many (noneOf "\n")
      return $ year ++ "/" ++ show month ++ "/" ++ day

readCommit :: String -> Int -> IO (Either String Commit)
readCommit path index = do
  let fileName = path ++ "/" ++ "commit" ++ show index ++ ".txt"
  fileExists <- doesFileExist fileName
  if fileExists
  then do
    content <- readFile fileName
    return . either (Left . show) Right $ parseCommit content
  else return $ Left "File does not exist"

readAllCommits :: String -> IO [Commit]
readAllCommits path = readAllCommits' 0 []
  where 
    readAllCommits' :: Int -> [Commit] -> IO [Commit]
    readAllCommits' index commits =
      readCommit path index >>= either done loop
      where  
        done err = do
          putStrLn $ "Parsing failed at index " ++ show index
          putStrLn $ "With error: " ++ show err
          return commits
         
        loop commit = 
          readAllCommits' (index + 1) (commits ++ [commit])

parseMonth :: String -> Maybe Int
parseMonth s = 
  snd <$> find (\e -> fst e == s) indexed
  where
    indexed = zip months [1..] :: [(String, Int)]
    months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
