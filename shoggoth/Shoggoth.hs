module Shoggoth where

import Data.Function ((&))
import Data.Text (Text)
import Shoggoth.Prelude (Action, implicitIndexFile, makeRelative, need, relativizeUrl, (</>))
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

-- | Get the default metadata for the website.
--
--   The default site metadata, plus:
--
--     - @build_date@: The date at which the is website was last built, in the RFC822 format.
getSiteMetadata ::
  FilePath -> -- TODO: create Config object
  Action Metadata
getSiteMetadata siteYml = do
  siteMetadata <- readYaml' siteYml
  buildDateFld <- currentDateField rfc822DateFormat "build_date"
  let metadata = mconcat [siteMetadata, buildDateFld]
  return $ constField "site" metadata

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
getFileWithMetadata ::
  ( ?routingTable :: RoutingTable,
    ?getSiteMetadata :: () -> Action Metadata
  ) =>
  FilePath -> -- TODO: create Config object
  FilePath ->
  Action (Metadata, Text)
getFileWithMetadata outDir src = do
  out <- route src
  let url = "/" <> makeRelative outDir out
  siteMetadata <- ?getSiteMetadata ()
  (fileMetadata, body) <- readFileWithMetadata' src
  let urlFld = constField "url" url
  let bodyFld = constField "body" body
  let sourceFld = constField "source" src
  modifiedDateFld <- lastModifiedISO8601Field src "modified_date"
  let metadata = mconcat [siteMetadata, fileMetadata, urlFld, bodyFld, sourceFld, modifiedDateFld]
  return (metadata, body)

-- | Get a template from the @templateDir@ directory.
getTemplateFile ::
  FilePath -> -- TODO: create Config object
  FilePath ->
  Action Template
getTemplateFile templateDir inputFile = do
  let inputPath = templateDir </> inputFile
  need [inputPath]
  compileTemplateFile inputPath
