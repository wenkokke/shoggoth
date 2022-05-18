module Shoggoth where

import Control.Monad (forM)
import Data.Default.Class (Default)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Shoggoth.Prelude (Action, Rules, implicitIndexFile, makeRelative, need, newCache, readFile', relativizeUrl, (</>), takeBaseName)
import Shoggoth.Routing (RoutingTable, route, routeAnchor, sources, Anchor)
import Shoggoth.Template.Metadata (Metadata, constField, currentDateField, htmlTeaserField, lastModifiedISO8601Field, postDateField, readFileWithMetadata', readYaml', rfc822DateFormat, textTeaserField)
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
-- Getters

data DefaultMetadataConfig = DefaultMetadataConfig
  { defaultMetadataFiles :: [FilePath],
    includeBuildDate :: Maybe Text
  }

instance Default DefaultMetadataConfig where
  def =
    DefaultMetadataConfig
      { defaultMetadataFiles = [],
        includeBuildDate = Just "build_date"
      }

-- | Get the default metadata for the website.
--
--   The default site metadata, plus:
--
--     - @build_date@: The date at which the is website was last built, in the RFC822 format.
makeDefaultMetadataGetter ::
  DefaultMetadataConfig ->
  Rules (() -> Action Metadata)
makeDefaultMetadataGetter DefaultMetadataConfig {..} = newCache $ \() -> do
  defaultMetadata <- forM defaultMetadataFiles $ \defaultMetadataFile -> do
    scopeMetadata <- readYaml' defaultMetadataFile
    let scopeName = T.pack $ takeBaseName defaultMetadataFile
    return $ constField scopeName (scopeMetadata :: Metadata)
  buildDateFld <-
    maybe (return mempty) (currentDateField rfc822DateFormat) includeBuildDate
  return $ mconcat defaultMetadata <> buildDateFld

data FileWithMetadataConfig = FileWithMetadataConfig
  {
  }

instance Default FileWithMetadataConfig where
  def =
    FileWithMetadataConfig
      {
      }

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
  ( ?outputDirectory :: FilePath,
    ?routingTable :: RoutingTable,
    ?getDefaultMetadata :: () -> Action Metadata
  ) =>
  FileWithMetadataConfig ->
  Rules (FilePath -> Action (Metadata, Text))
makeFileWithMetadataGetter FileWithMetadataConfig {} = newCache $ \src -> do
  out <- route src
  let url = "/" <> makeRelative ?outputDirectory out
  siteMetadata <- ?getDefaultMetadata ()
  (fileMetadata, body) <- readFileWithMetadata' src
  let urlFld = constField "url" url
  let bodyFld = constField "body" body
  let sourceFld = constField "source" src
  modifiedDateFld <- lastModifiedISO8601Field src "modified_date"
  let metadata = mconcat [siteMetadata, fileMetadata, urlFld, bodyFld, sourceFld, modifiedDateFld]
  return (metadata, body)

newtype TemplateFileConfig = TemplateFileConfig
  { templateDirectory :: FilePath
  }

instance Default TemplateFileConfig where
  def =
    TemplateFileConfig
      { templateDirectory = "templates"
      }

-- | Get a template from the template directory.
makeTemplateFileGetter ::
  TemplateFileConfig ->
  Rules (FilePath -> Action Template)
makeTemplateFileGetter TemplateFileConfig {..} = newCache $ \inputFile -> do
  let inputPath = templateDirectory </> inputFile
  need [inputPath]
  compileTemplateFile inputPath

newtype PostWithMetadataConfig = PostWithMetadataConfig
  { postDateFromFilePath :: Bool
  }

instance Default PostWithMetadataConfig where
  def =
    PostWithMetadataConfig
      { postDateFromFilePath = True
      }

-- | Get a metadata object representing all posts.
--
--   This function adds the following metadata fields for each post,
--   in addition to the metadata added by 'getFileWithMetadata':
--
--   - @date@: The date of the post, in a human readable format.
--   - @date_rfc822@: The date of the post, in the RFC822 date format.
makePostWithMetadataGetter ::
  ( ?outputDirectory :: FilePath,
    ?routingTable :: RoutingTable,
    ?getFileWithMetadata :: FilePath -> Action (Metadata, Text)
  ) =>
  PostWithMetadataConfig ->
  Rules (FilePath -> Action (Metadata, Text))
makePostWithMetadataGetter PostWithMetadataConfig {..} = newCache $ \src -> do
  (fileMetadata, body) <- ?getFileWithMetadata src
  postDateMetadata <-
    if not postDateFromFilePath
      then mempty
      else do
        dateFld <- either fail return $ postDateField "%a %-d %b, %Y" src "date"
        dateRfc822Fld <- either fail return $ postDateField rfc822DateFormat src "date_rfc822"
        return $ mconcat [dateFld, dateRfc822Fld]
  let metadata = mconcat [fileMetadata, postDateMetadata]
  return (metadata, body)

data PostListMetadataConfig = PostListMetadataConfig
  { isPostSource :: FilePath -> Bool,
    includeBodyFromAnchor :: Maybe Anchor,
    includeTeaser :: Bool
  }

instance Default PostListMetadataConfig where
  def =
    PostListMetadataConfig
      { isPostSource = const True,
        includeBodyFromAnchor = Nothing,
        includeTeaser = False
      }

-- | Get a metadata object representing all posts.
--
--   This function adds the following metadata fields for each post,
--   in addition to the metadata added by 'getFileWithMetadata':
--
--   - @body_html@: The rendered HTML body of the source file.
--   - @teaser_html@: The rendered HTML teaser for the post.
--   - @teaser_plain@: The plain text teaser for the post.
makePostListMetadataGetter ::
  ( ?outputDirectory :: FilePath,
    ?routingTable :: RoutingTable
  ) =>
  (FilePath -> Action (Metadata, Text)) ->
  PostListMetadataConfig ->
  Rules (() -> Action Metadata)
makePostListMetadataGetter getPostWithMetadata PostListMetadataConfig {..} = newCache $ \() -> do
  -- Get posts from routing table
  postSrcs <- filter isPostSource <$> sources
  -- Gather metadata for each post
  postsMetadata <- forM postSrcs $ \src -> do

    -- Get output file for URL
    out <- route src
    let url = "/" <> makeRelative ?outputDirectory out
    postMetadata <- fst <$> getPostWithMetadata src

    -- Include body (optionally from anchor)
    (body, bodyFldName) <-
      case includeBodyFromAnchor of
        Nothing -> do
          body <- readFile' src
          return (body, "body")
        Just bodyAnchor -> do
          body <- readFile' =<< routeAnchor bodyAnchor src
          return (body, bodyAnchor)
    let bodyMetadata = constField bodyFldName body

    -- Include teaser (derived from body)
    teaserMetadata <-
      if not includeTeaser
        then mempty
        else do
          -- Assumes body is HTML
          teaserHtmlFld <- either fail return $ htmlTeaserField url body "teaser_html"
          teaserPlainFld <- either fail return $ textTeaserField body "teaser_plain"
          return $ mconcat [teaserHtmlFld, teaserPlainFld]
    return $ mconcat [postMetadata, bodyMetadata, teaserMetadata]

  return $ constField "post" (reverse postsMetadata)
