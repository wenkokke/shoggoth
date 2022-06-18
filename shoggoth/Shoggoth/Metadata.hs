module Shoggoth.Metadata
  ( Metadata (..),
    toMetadata,
    readYaml,
    readYamlIO,
    writeYaml,
    writeYamlIO,
    readYamlFrontmatter,
    readYamlFrontmatterIO,
    readFileWithMetadata,
    readFileWithMetadataIO,
    (^.),
    (.~),
    (&),
    lastModifiedISO8601Field,
    postDateField,
    currentDateField,
    constField,
    titleVariantMetadata,
    resolveIncludes,
    rfc822DateFormat,
    htmlTeaserFieldFromHtml,
    textTeaserFieldFromHtml
  )
where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (encode, encodeFile)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types as Aeson
  ( FromJSON (parseJSON),
    Object,
    Result (..),
    ToJSON (toJSON),
    Value (Array, Object),
    fromJSON,
    withObject,
  )
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Foldable (foldlM, foldrM)
import Data.Frontmatter qualified as Frontmatter
import Data.Function ((&))
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Encoding qualified as LazyText
import Data.Time (LocalTime (LocalTime), UTCTime, defaultTimeLocale, formatTime, fromGregorian, getCurrentTime, localTimeToUTC, midday, utc)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Vector qualified as Vector (fromList)
import Data.Yaml qualified as Yaml
import Shoggoth.Configuration
import Shoggoth.PostInfo qualified as PostInfo
import Shoggoth.Prelude
import Shoggoth.TagSoup qualified as TagSoup
import Shoggoth.Template.Pandoc.Builder qualified as Builder
import System.Directory (getModificationTime)
import System.IO.Unsafe (unsafePerformIO)
import Text.Pandoc.Citeproc qualified as Citeproc (getReferences)
import Text.Printf (printf)
import GHC.Stack.Types (HasCallStack)

--------------------------------------------------------------------------------
-- Metadata
--------------------------------------------------------------------------------

newtype Metadata = Metadata Object

infixr 4 .~

-- | Set a value on a metadata object.
(.~) :: ToJSON a => Text -> a -> Metadata -> Metadata
(key .~ value) (Metadata obj) = Metadata $ KeyMap.insert (Key.fromText key) (toJSON value) obj

infixl 8 ^.

-- | Get a value from a metadata object.
(^.) :: (MonadError String m, FromJSON a) => Metadata -> Text -> m a
Metadata obj ^. keyOrKeys = do
  let keys = Text.splitOn "." keyOrKeys
  val <- foldlM getOrFail (Object obj) keys
  liftAesonResult (fromJSON val)
  where
    get :: (MonadError String m, FromJSON a) => Metadata -> Text -> m a
    get metadata@(Metadata obj) key =
      case KeyMap.lookup (Key.fromText key) obj of
        Nothing -> throwError $ printf "Key '%s' not found in:\n%s" (Text.unpack key) (show metadata)
        Just val -> liftAesonResult (fromJSON val)

    getOrFail :: MonadError String m => Value -> Text -> m Value
    getOrFail val key = case val of
      Object obj -> get (Metadata obj) key
      val -> throwError $ printf "Cannot lookup key '%s' in:\n" (Text.unpack key) (show val)

    liftAesonResult :: (MonadError String m) => Aeson.Result a -> m a
    liftAesonResult = \case
      Aeson.Error msg -> throwError msg
      Aeson.Success a -> return a

toMetadata :: HasCallStack => ToJSON a => a -> Metadata
toMetadata a = case toJSON a of
  Object obj -> Metadata obj
  v -> error $ printf "Expected object, got '%s'" (show v)

--------------------------------------------------------------------------------
-- Reading and writing
--------------------------------------------------------------------------------

-- | Read a YAML file as a Shake action.
readYaml :: FromJSON a => FilePath -> Action a
readYaml inputFile = do
  need [inputFile]
  liftIO $ readYamlIO inputFile

-- | Read a YAML file.
readYamlIO :: FromJSON a => FilePath -> IO a
readYamlIO = Yaml.decodeFileThrow

-- | Read the YAML frontmatter from a file as a Shake action.
readYamlFrontmatter :: FilePath -> Action Metadata
readYamlFrontmatter inputFile = do
  need [inputFile]
  readYamlFrontmatterIO inputFile

-- | Read the YAML frontmatter from a file.
readYamlFrontmatterIO :: MonadIO m => FilePath -> m Metadata
readYamlFrontmatterIO inputFile = liftIO $ fst <$> readFileWithMetadataIO inputFile

-- | Read a file with its YAML frontmatter and return both as a Shake action.
readFileWithMetadata :: FilePath -> Action (Metadata, Text)
readFileWithMetadata inputFile = do
  need [inputFile]
  readFileWithMetadataIO inputFile

-- | Read a file with its YAML frontmatter and return both.
readFileWithMetadataIO :: MonadIO m => FilePath -> m (Metadata, Text)
readFileWithMetadataIO inputFile = liftIO $ do
  contents <- ByteString.readFile inputFile
  case liftFrontmatterResult (Frontmatter.parseYamlFrontmatter contents) of
    -- Parse succeeded
    Right (metadata, body) -> do
      return (metadata, Text.decodeUtf8 body)
    -- Parse failed; is there a header?
    Left errorMessage -> do
      let bodyText = Text.decodeUtf8 contents
      let hasHeader = "---" `Text.isPrefixOf` bodyText
      if hasHeader
        then error $ unlines ["Parse error in header: " <> inputFile, errorMessage]
        else return (mempty, bodyText)
  where
    liftFrontmatterResult :: (FromJSON a) => Frontmatter.Result a -> Either String (a, ByteString)
    liftFrontmatterResult (Frontmatter.Done body metadata) = Right (metadata, body)
    liftFrontmatterResult (Frontmatter.Fail _ [] errorMessage) = Left errorMessage
    liftFrontmatterResult (Frontmatter.Fail _ contexts errorMessage) = Left (List.intercalate " > " contexts ++ ": " ++ errorMessage)
    liftFrontmatterResult _ = Left "incomplete input"

writeYaml :: ToJSON a => FilePath -> a -> Action ()
writeYaml outputFile a = liftIO $ Yaml.encodeFile outputFile a

writeYamlIO :: ToJSON a => FilePath -> a -> IO ()
writeYamlIO = Yaml.encodeFile

-- * Instances

instance Show Metadata where
  show (Metadata obj) = LazyText.unpack (LazyText.decodeUtf8 $ encode obj)

instance ToJSON Metadata where
  toJSON (Metadata obj) = Object obj

instance FromJSON Metadata where
  parseJSON = withObject "Metadata" $ \v -> return (Metadata v)

instance Semigroup Metadata where
  (Metadata m1) <> (Metadata m2) = Metadata (KeyMap.union m1 m2)

instance Monoid Metadata where
  mempty = Metadata KeyMap.empty

--------------------------------------------------------------------------------
-- Metadata fields
--------------------------------------------------------------------------------

type DateFormat = String

-- | Variant of 'Data.Time.rfc822DateFormat' which actually conforms to RFC-822.
rfc822DateFormat :: DateFormat
rfc822DateFormat = "%a, %d %b %Y %H:%M:%S %z"

-- | Create a metadata object containing the file modification time.
--
--   Adapted from hakyll's 'Hakyll.Web.Template.Context.modificationTimeField'.
lastModifiedISO8601Field :: FilePath -> Text -> Action Metadata
lastModifiedISO8601Field inputFile key = liftIO $ do
  modificationTime <- getModificationTime inputFile
  return $ constField key (iso8601Show modificationTime)

-- | Create a metadata object containing the current date.
currentDateField :: DateFormat -> Text -> Action Metadata
currentDateField fmt key = do
  currentTime <- liftIO getCurrentTime
  let currentTimeString = formatTime defaultTimeLocale fmt currentTime
  return $ constField key currentTimeString

-- | Create a metadata object containing the date inferred from the file path.
postDateField :: MonadError String m => DateFormat -> FilePath -> Text -> m Metadata
postDateField fmt inputFile key = do
  let inputFileName = takeFileName inputFile
  postDate <- dateFromPostFileName inputFileName
  let postDateString = formatTime defaultTimeLocale fmt postDate
  return $ constField key postDateString

dateFromPostFileName :: MonadError String m => FilePath -> m UTCTime
dateFromPostFileName postFile = do
  postInfo <- PostInfo.parsePostSource (takeFileName postFile)
  let year = read $ PostInfo.postYear postInfo
  let month = read $ PostInfo.postMonth postInfo
  let day = read $ PostInfo.postDay postInfo
  let dateTime = LocalTime (fromGregorian year month day) midday
  return $ localTimeToUTC utc dateTime

-- | Create a metadata object containing the provided value.
constField :: ToJSON a => Text -> a -> Metadata
constField key a = mempty & key .~ a

-- | Create a metadata object containing an HTML teaser constructed from the first argument.
htmlTeaserFieldFromHtml :: MonadError String m => FilePath -> Text -> Text -> m Metadata
htmlTeaserFieldFromHtml teaserUrl htmlBody key = do
  let htmlBodyWithAbsoluteUrls = makeAnchorsAbsolute teaserUrl htmlBody
  -- NOTE: specific to Pandoc footnote IDs
  let htmlBodyWithoutFootnoteAnchorIds = TagSoup.removeFootnoteAnchorId htmlBodyWithAbsoluteUrls
  extractTeaserHtml <- extractTeaserHtml htmlBodyWithoutFootnoteAnchorIds
  return $ constField key extractTeaserHtml

-- | Create a metadata object containing a plain-text teaser constructed from the first argument.
textTeaserFieldFromHtml :: MonadError String m => Text -> Text -> m Metadata
textTeaserFieldFromHtml htmlBody key = do
  htmlTeaser <- extractTeaserHtml htmlBody
  let teaser = TagSoup.stripTags htmlTeaser
  return $ constField key teaser

-- | Ensure that anchor URLs (e.g., "#README") to absolute URLs.
--
--   This is needed when embeddding part of one HTML document into
--   another, e.g., for blog post teasers.
makeAnchorsAbsolute :: FilePath -> Text -> Text
makeAnchorsAbsolute absoluteUrl =
  TagSoup.withUrls $ \url ->
    if "#" `Text.isPrefixOf` url then Text.pack absoluteUrl <> url else url

-- | Extract the teaser from an HTML document.
--
--   NOTE: Takes the prefix up to '<!--more-->', which does not work if the
--         prefix is not valid, e.g., for standalone HTML documents.
extractTeaserHtml :: MonadError String m => Text -> m Text
extractTeaserHtml body =
  let (teaser, rest) = Text.breakOn "<!--more-->" body
   in if not (Text.null rest) then return teaser else throwError "Delimiter '<!--more-->' not found"

-- | Add running title and subtitle, if title contains a colon.
titleVariantMetadata :: Text -> Metadata
titleVariantMetadata title =
  let (titlerunning, subtitle) = Text.breakOn ":" title
   in if Text.null subtitle
        then mempty -- No titlerunning/subtitle distinction
        else
          mempty
            & "titlerunning" .~ Text.strip titlerunning
            & "subtitle" .~ Text.strip (Text.drop 1 subtitle)

-- | Resolve 'include' fields by including metadata from files.
resolveIncludes :: (FilePath -> Action Metadata) -> Metadata -> Action Metadata
resolveIncludes reader (Metadata object) = fromValue <$> walkValue (Object object)
  where
    fromValue :: Value -> Metadata
    fromValue (Object object) = Metadata object
    fromValue v = error $ "Unexpected value " <> show v

    walkValue :: Value -> Action Value
    walkValue (Object object) = do
      object' <- resolveInclude object
      Object <$> traverse walkValue object'
    walkValue (Array array) = Array <$> traverse walkValue array
    walkValue value = return value

    resolveInclude :: Object -> Action Object
    resolveInclude object =
      case Metadata object ^. "include" of
        Left _e -> return object
        Right filePath -> do
          Metadata includedObject <- reader filePath
          return (object <> includedObject)