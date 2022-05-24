module Shoggoth.PostInfo (PostInfo (..), parsePostSource, parsePostOutput) where

import Control.Monad.Except (MonadError (throwError))
import Data.Char (isAlphaNum, isDigit)
import Data.Functor (($>))
import System.FilePath (splitFileName, dropTrailingPathSeparator)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    count,
    eof,
    many1,
    munch1,
    readP_to_S,
    satisfy,
    string,
  )

data PostInfo = PostInfo
  { postDirectory :: FilePath,
    postYear :: String,
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
parsePostSource filePath =
  let (directory, fileName) = splitFileName filePath
   in runReadP (pPostSource $ dropTrailingPathSeparator directory) fileName

pPostSource :: FilePath -> ReadP PostInfo
pPostSource directory =
  PostInfo directory <$> pYear <* char '-' <*> pMonth <* char '-' <*> pDay <* char '-' <*> pSlug <*> pFileExts <* eof
  where
    pYear = count 4 (satisfy isDigit)
    pMonth = count 2 (satisfy isDigit)
    pDay = count 2 (satisfy isDigit)
    pSlug = munch1 (\c -> isAlphaNum c || c == '-')
    pFileExts = many1 (char '.' *> munch1 isAlphaNum)

parsePostOutput :: MonadError String m => FilePath -> m PostInfo
parsePostOutput filePath =
  let (directory, fileName) = splitFileName filePath
   in runReadP (pPostOutput $ dropTrailingPathSeparator directory) fileName

pPostOutput :: FilePath -> ReadP PostInfo
pPostOutput directory =
  PostInfo directory <$> pYear <* char '/' <*> pMonth <* char '/' <*> pDay <* char '/' <*> pSlug <* char '/' <*> pIndexHtml <* eof
  where
    pYear = count 4 (satisfy isDigit)
    pMonth = count 2 (satisfy isDigit)
    pDay = count 2 (satisfy isDigit)
    pSlug = munch1 (\c -> isAlphaNum c || c == '-')
    pIndexHtml = string "index.html" $> []