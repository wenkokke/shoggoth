module Shoggoth.Prelude
  ( readFile',
    writeFile',
    hasExecutable,
    isRunningOnCI,
    liftEither,
    module Export
  ) where

import Development.Shake as Export hiding (readFile', writeFile')
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Shoggoth.Prelude.FilePath as Export
import Shoggoth.Prelude.Url as Export
import System.Directory
import Control.Monad (join)
import Data.Maybe (isJust)
import System.Environment (lookupEnv)

readFile' :: FilePath -> Action Text
readFile' fp = need [fp] >> liftIO (T.readFile fp)

writeFile' :: FilePath -> Text -> Action ()
writeFile' fp content = liftIO $ do
  createDirectoryIfMissing True (takeDirectory fp)
  removeFile_ fp
  T.writeFile fp content

hasExecutable :: String -> Action Bool
hasExecutable prog = isJust <$> liftIO (findExecutable prog)

isRunningOnCI :: Action Bool
isRunningOnCI = liftIO $ (Just "true" ==) <$> lookupEnv "CI"

{-# INLINE liftEither #-}
liftEither :: MonadFail m => (e -> String) -> Either e a -> m a
liftEither pretty (Left e) = fail (pretty e)
liftEither _ (Right a) = return a