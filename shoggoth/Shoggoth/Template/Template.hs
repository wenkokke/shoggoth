module Shoggoth.Template.Template where

import Shoggoth.Template.Metadata (Metadata (Metadata), constField)
import Shoggoth.Prelude (Action, liftIO, readFile', liftEither)
import Text.DocLayout as Doc (render)
import Text.Pandoc.Templates qualified as Template
import Control.Monad (foldM)
import Data.Aeson (Value(Object))
import Data.Text (Text)

type Template = Template.Template Text

compileTemplate :: FilePath -> Text -> Action Template
compileTemplate filepath contents = do
  tplOrError <- liftIO (Template.compileTemplate filepath contents)
  liftEither id tplOrError

compileTemplateFile :: FilePath -> Action Template
compileTemplateFile filepath = do
  contents <- readFile' filepath
  compileTemplate filepath contents

renderTemplate :: Template -> Metadata -> Text
renderTemplate template (Metadata obj) =
  Doc.render Nothing (Template.renderTemplate template (Object obj))

applyAsTemplate ::
  Metadata ->
  Text ->
  Action Text
applyAsTemplate metadata template = do
  tpl <- compileTemplate "" template
  return $ renderTemplate tpl metadata

applyTemplate ::
  ( ?getTemplateFile :: FilePath -> Action Template
  ) =>
  FilePath ->
  Metadata ->
  Text ->
  Action Text
applyTemplate templateFile metadata body = do
  template <- ?getTemplateFile templateFile
  return $ renderTemplate template (constField "body" body <> metadata)

applyTemplates ::
  ( ?getTemplateFile :: FilePath -> Action Template
  ) =>
  [FilePath] ->
  Metadata ->
  Text ->
  Action Text
applyTemplates templateFiles metadata body =
  foldM (\body templateFile -> applyTemplate templateFile metadata body) body templateFiles
