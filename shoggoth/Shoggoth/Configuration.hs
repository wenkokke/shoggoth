module Shoggoth.Configuration where

import Data.Default.Class (Default (def))
import Data.Maybe
import Shoggoth.Prelude

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
