module Shoggoth where

import Data.Default.Class (Default)
import Data.Function ((&))
import Data.Text (Text)
import Shoggoth.Prelude (Action, Rules, implicitIndexFile, makeRelative, need, newCache, relativizeUrl, (</>))
import Shoggoth.Routing (RoutingTable, route)
import Shoggoth.Template.Metadata (Metadata, constField, currentDateField, lastModifiedISO8601Field, readFileWithMetadata', readYaml', rfc822DateFormat)
import Shoggoth.Template.Pandoc (Pandoc)
import Shoggoth.Template.Pandoc qualified as Pandoc
import Shoggoth.Template.Pandoc.Builder qualified as Builder
import Shoggoth.Template.Pandoc.Citeproc qualified as Citeproc
import Shoggoth.Template.TagSoup qualified as TagSoup
import Shoggoth.Template.Template (Template, compileTemplateFile)

--------------------------------------------------------------------------------
-- Markdown to HTML compilation

markdownToPandoc ::
  ( ?readerOpts :: Pandoc.ReaderOptions
  ) =>
  Text ->
  Action Pandoc
markdownToPandoc =
  Pandoc.runPandoc . Pandoc.readMarkdown ?readerOpts

pandocToHtml5 ::
  ( ?writerOpts :: Pandoc.WriterOptions
  ) =>
  Pandoc ->
  Action Text
pandocToHtml5 =
  Pandoc.runPandoc . Pandoc.writeHtml5String ?writerOpts

processCitations :: Pandoc -> Action Pandoc
processCitations =
  Pandoc.runPandoc . Citeproc.processCitations

postprocessHtml5 :: FilePath -> FilePath -> Text -> Text
postprocessHtml5 outDir out html5 =
  html5
    & TagSoup.withUrls (implicitIndexFile . relativizeUrl outDir out)
    & TagSoup.addDefaultTableHeaderScope "col"
    & Pandoc.postprocessHtml5

--------------------------------------------------------------------------------
-- Metadata

newtype SiteMetadataConfig = SiteMetadataConfig {siteYaml :: FilePath}

instance Default SiteMetadataConfig where
  def = SiteMetadataConfig {siteYaml = "site.yaml"}

-- | Get the default metadata for the website.
--
--   The default site metadata, plus:
--
--     - @build_date@: The date at which the is website was last built, in the RFC822 format.
makeSiteMetadataGetter ::
  SiteMetadataConfig ->
  Rules (() -> Action Metadata)
makeSiteMetadataGetter SiteMetadataConfig {..} = newCache $ \() -> do
  siteMetadata <- readYaml' siteYaml
  buildDateFld <- currentDateField rfc822DateFormat "build_date"
  let metadata = mconcat [siteMetadata, buildDateFld]
  return $ constField "site" metadata

newtype FileWithMetadataConfig = FileWithMetadataConfig {outputDirectory :: FilePath}

instance Default FileWithMetadataConfig where
  def = FileWithMetadataConfig {outputDirectory = "out"}

-- | Get a file body and its metadata, including derived metadata.
--
--   This function adds the following metadata fields,
--   in addition to the metadata added by 'getSiteMetadata':
--
--     - @url@: The URL to the output file.
--     - @body@: The body of the source file.
--     - @source@: The path to the source file.
--     - @modified_date@: The date at which the file was last modified, in the ISO8601 format.
--     - @build_date@: The date at which the is website was last built, in the RFC822 format.
makeFileWithMetadataGetter ::
  ( ?routingTable :: RoutingTable,
    ?getSiteMetadata :: () -> Action Metadata
  ) =>
  FileWithMetadataConfig ->
  Rules (FilePath -> Action (Metadata, Text))
makeFileWithMetadataGetter FileWithMetadataConfig {..} = newCache $ \src -> do
  out <- route src
  let url = "/" <> makeRelative outputDirectory out
  siteMetadata <- ?getSiteMetadata ()
  (fileMetadata, body) <- readFileWithMetadata' src
  let urlFld = constField "url" url
  let bodyFld = constField "body" body
  let sourceFld = constField "source" src
  modifiedDateFld <- lastModifiedISO8601Field src "modified_date"
  let metadata = mconcat [siteMetadata, fileMetadata, urlFld, bodyFld, sourceFld, modifiedDateFld]
  return (metadata, body)

newtype TemplateFileConfig = TemplateFileConfig {templateDirectory :: FilePath}

instance Default TemplateFileConfig where
  def = TemplateFileConfig {templateDirectory = "templates"}

-- | Get a template from the template directory.
makeTemplateFileGetter ::
  TemplateFileConfig ->
  Rules (FilePath -> Action Template)
makeTemplateFileGetter TemplateFileConfig {..} = newCache $ \inputFile -> do
  let inputPath = templateDirectory </> inputFile
  need [inputPath]
  compileTemplateFile inputPath
