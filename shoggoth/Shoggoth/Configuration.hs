module Shoggoth.Configuration where

import Data.Default.Class (Default (def))
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Shoggoth.Prelude (Action, getEnvWithDefault, getShakeExtra)

newtype CacheDirectory = CacheDirectory {fromCacheDirectory :: FilePath}

instance Default CacheDirectory where
  def = CacheDirectory "tmp"

getCacheDirectory :: Action FilePath
getCacheDirectory = do
  maybeCacheDirectory <- getShakeExtra @CacheDirectory
  return . fromCacheDirectory . fromMaybe def $ maybeCacheDirectory

newtype OutputDirectory = OutputDirectory {fromOutputDirectory :: FilePath}

instance Default OutputDirectory where
  def = OutputDirectory "out"

getOutputDirectory :: Action FilePath
getOutputDirectory = do
  maybeOutputDirectory <- getShakeExtra @OutputDirectory
  return . fromOutputDirectory . fromMaybe def $ maybeOutputDirectory

newtype TemplateDirectory = TemplateDirectory {fromTemplateDirectory :: FilePath}

instance Default TemplateDirectory where
  def = TemplateDirectory "templates"

getTemplateDirectory :: Action FilePath
getTemplateDirectory = do
  maybeTemplateDirectory <- getShakeExtra @TemplateDirectory
  return . fromTemplateDirectory . fromMaybe def $ maybeTemplateDirectory

data Mode = Production | Development
  deriving (Eq, Show)

getMode :: Action Mode
getMode = do
  modeString <- getEnvWithDefault "production" "MODE"
  if "dev" `List.isPrefixOf` modeString
    then return Development
    else return Production
