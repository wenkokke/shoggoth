module Shoggoth.PostInfo (PostInfo (..), parsePostSource, parsePostOutput) where

import Control.Monad.Except (MonadError (throwError))
import Data.Char (isAlphaNum, isDigit)
import Data.Functor (($>))
import System.FilePath (dropTrailingPathSeparator, joinPath, makeRelative, pathSeparator, splitFileName)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    count,
    eof,
    many,
    many1,
    munch1,
    readP_to_S,
    satisfy,
    string,
  )

data PostInfo = PostInfo
  { postYear :: String,
    postMonth :: String,
    postDay :: String,
    postSlug :: String,
    postFileExtensions :: [String]
  }
  deriving (Show)

runReadP :: MonadError String m => ReadP a -> String -> m a
runReadP p str = case readP_to_S p str of
  [] -> throwError $ "No parse: " <> str
  [(a, "")] -> return a
  (_ : _) -> throwError $ "Ambiguous parse: " <> str

parsePostSource :: MonadError String m => FilePath -> m PostInfo
parsePostSource = runReadP pPostSource

pPostSource :: ReadP PostInfo
pPostSource =
  PostInfo
    <$> pYear <* char '-'
    <*> pMonth <* char '-'
    <*> pDay <* char '-'
    <*> pSlug
    <*> pFileExts <* eof

parsePostOutput :: MonadError String m => FilePath -> m PostInfo
parsePostOutput = runReadP pPostOutput

pPostOutput :: ReadP PostInfo
pPostOutput =
  PostInfo
    <$> pYear <* char pathSeparator
    <*> pMonth <* char pathSeparator
    <*> pDay <* char pathSeparator
    <*> pSlug <* char pathSeparator
    <*> (string "index.html" $> []) <* eof

pYear, pMonth, pDay :: ReadP String
pYear = count 4 (satisfy isDigit)
pMonth = count 2 (satisfy isDigit)
pDay = count 2 (satisfy isDigit)

pSlug :: ReadP String
pSlug = munch1 (\c -> isAlphaNum c || c == '-')

pFileExts :: ReadP [String]
pFileExts = many1 (char '.' *> munch1 isAlphaNum)

pFilePath :: ReadP FilePath
pFilePath = joinPath <$> many (many1 (satisfy (/= pathSeparator) <* char pathSeparator))