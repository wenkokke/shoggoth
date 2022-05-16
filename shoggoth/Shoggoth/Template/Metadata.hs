module Shoggoth.Template.Metadata
  ( Metadata (..),
    readYaml',
    readYaml,
    writeYaml',
    writeYaml,
    readYamlFrontmatter',
    readYamlFrontmatter,
    readFileWithMetadata',
    readFileWithMetadata,
    (^.),
    (.~),
    (&),
    lastModifiedISO8601Field,
    postDateField,
    currentDateField,
    constField,
    htmlTeaserField,
    textTeaserField,
    addTitleVariants,
    resolveIncludes,
    rfc822DateFormat,
    permalinkRouter,
  )
where

import Shoggoth.PostInfo qualified as PostInfo
import Shoggoth.Prelude
import Shoggoth.Prelude.ByteString qualified as ByteString
import Shoggoth.Template.Pandoc.Builder qualified as Builder
import Shoggoth.Template.TagSoup qualified as TagSoup
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (encode, encodeFile)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Frontmatter qualified as Frontmatter
import Data.Function ((&))
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (LocalTime (LocalTime), UTCTime, defaultTimeLocale, formatTime, fromGregorian, getCurrentTime, localTimeToUTC, midday, utc)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Vector qualified as Vector (fromList)
import Data.Yaml qualified as Yaml
import System.Directory (getModificationTime)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf
import Text.Pandoc.Citeproc qualified as Citeproc (getReferences)
import Data.Foldable (foldrM, foldlM)
import Control.Monad.Except (MonadError (throwError))

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

--------------------------------------------------------------------------------
-- Reading and writing
--------------------------------------------------------------------------------

-- | Read a YAML file as a Shake action.
readYaml' :: FromJSON a => FilePath -> Action a
readYaml' inputFile = do
  need [inputFile]
  liftIO $ readYaml inputFile

-- | Read a YAML file.
readYaml :: FromJSON a => FilePath -> IO a
readYaml = Yaml.decodeFileThrow

-- | Read the YAML frontmatter from a file as a Shake action.
readYamlFrontmatter' :: FilePath -> Action Metadata
readYamlFrontmatter' inputFile = do
  need [inputFile]
  readYamlFrontmatter inputFile

-- | Read the YAML frontmatter from a file.
readYamlFrontmatter :: MonadIO m => FilePath -> m Metadata
readYamlFrontmatter inputFile =
  liftIO $
    fst <$> readFileWithMetadata inputFile

-- | Read a file with its YAML frontmatter and return both as a Shake action.
readFileWithMetadata' :: FilePath -> Action (Metadata, Text)
readFileWithMetadata' inputFile = do
  need [inputFile]
  readFileWithMetadata inputFile

-- | Read a file with its YAML frontmatter and return both.
readFileWithMetadata :: MonadIO m => FilePath -> m (Metadata, Text)
readFileWithMetadata inputFile = liftIO $ do
  contents <- ByteString.readFile inputFile
  case liftFrontmatterResult (Frontmatter.parseYamlFrontmatter contents) of
    -- Parse succeeded
    Right (metadata, body) -> do
      body <- ByteString.toText body
      return (metadata, body)
    -- Parse failed; is there a header?
    Left errorMessage -> do
      body <- ByteString.toText contents
      let hasHeader = "---" `Text.isPrefixOf` body
      if hasHeader
        then error $ unlines ["Parse error in header: " <> inputFile, errorMessage]
        else return (mempty, body)
  where
    liftFrontmatterResult :: (FromJSON a) => Frontmatter.Result a -> Either String (a, ByteString)
    liftFrontmatterResult (Frontmatter.Done body metadata) = Right (metadata, body)
    liftFrontmatterResult (Frontmatter.Fail _ [] errorMessage) = Left errorMessage
    liftFrontmatterResult (Frontmatter.Fail _ contexts errorMessage) = Left (List.intercalate " > " contexts ++ ": " ++ errorMessage)
    liftFrontmatterResult _ = Left "incomplete input"

writeYaml' :: ToJSON a => FilePath -> a -> Action ()
writeYaml' outputFile a = liftIO $ Yaml.encodeFile outputFile a

writeYaml :: ToJSON a => FilePath -> a -> IO ()
writeYaml = Yaml.encodeFile

-- * Instances

instance Show Metadata where
  show (Metadata obj) = Text.unpack $ unsafePerformIO $ ByteString.toTextLazy $ encode obj

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

-- | Variant of 'Data.Time.rfc822DateFormat' which actually conforms to RFC-822.
rfc822DateFormat :: String
rfc822DateFormat = "%a, %d %b %Y %H:%M:%S %z"

-- | Create a metadata object containing the file modification time.
--
--   Adapted from hakyll's 'Hakyll.Web.Template.Context.modificationTimeField'.
lastModifiedISO8601Field :: FilePath -> Text -> Action Metadata
lastModifiedISO8601Field inputFile key = liftIO $ do
  modificationTime <- getModificationTime inputFile
  return $ constField key (iso8601Show modificationTime)

-- | Create a metadata object containing the current date.
currentDateField :: String -> Text -> Action Metadata
currentDateField fmt key = do
  currentTime <- liftIO getCurrentTime
  let currentTimeString = formatTime defaultTimeLocale fmt currentTime
  return $ constField key currentTimeString

-- | Create a metadata object containing the date inferred from the file path.
postDateField :: MonadError String m => String -> FilePath -> Text -> m Metadata
postDateField fmt inputFile key = do
  let inputFileName = takeFileName inputFile
  postDate <- dateFromPostFileName inputFileName
  let postDateString = formatTime defaultTimeLocale fmt postDate
  return $ constField key postDateString

dateFromPostFileName :: MonadError String m => FilePath -> m UTCTime
dateFromPostFileName postFile = do
  postInfo <- PostInfo.parsePostSource postFile
  let year = read $ PostInfo.year postInfo
  let month = read $ PostInfo.month postInfo
  let day = read $ PostInfo.day postInfo
  let dateTime = LocalTime (fromGregorian year month day) midday
  return $ localTimeToUTC utc dateTime

-- | Create a metadata object containing the provided value.
constField :: ToJSON a => Text -> a -> Metadata
constField key a = mempty & key .~ a

-- | Create a metadata object containing an HTML teaser constructed from the first argument.
htmlTeaserField :: MonadError String m => FilePath -> Text -> Text -> m Metadata
htmlTeaserField teaserUrl htmlBody key = do
  let htmlBodyWithAbsoluteUrls = htmlTeaserFixUrl teaserUrl htmlBody
  let htmlBodyWithoutFootnoteAnchorIds = TagSoup.removeFootnoteAnchorId htmlBodyWithAbsoluteUrls
  htmlTeaserBody <- htmlTeaserBody htmlBodyWithoutFootnoteAnchorIds
  return $ constField key htmlTeaserBody

-- | Create a metadata object containing a plain-text teaser constructed from the first argument.
textTeaserField :: MonadError String m => Text -> Text -> m Metadata
textTeaserField htmlBody key = do
  htmlTeaser <- htmlTeaserBody htmlBody
  let teaser = TagSoup.stripTags htmlTeaser
  return $ constField key teaser

htmlTeaserFixUrl :: FilePath -> Text -> Text
htmlTeaserFixUrl teaserUrl = TagSoup.withUrls $ \url ->
  if "#" `Text.isPrefixOf` url
    then Text.pack teaserUrl <> url
    else url

htmlTeaserBody :: MonadError String m => Text -> m Text
htmlTeaserBody body
  | Text.null rest = throwError "Delimiter '<!--more-->' not found"
  | otherwise = return teaser
  where
    (teaser, rest) = Text.breakOn "<!--more-->" body

-- | Add running title and subtitle, if title contains a colon.
addTitleVariants :: Metadata -> Metadata
addTitleVariants metadata = case metadata ^. "title" of
  Left _e -> metadata
  Right title ->
    let (titlerunning, subtitle) = Text.breakOn ":" title
     in if Text.null subtitle
          then metadata -- No titlerunning/subtitle distinction
          else
            metadata
              & "titlerunning" .~ Text.strip titlerunning
              & "subtitle" .~ Text.strip (Text.drop 1 subtitle)

-- | Route files based on their permalink.
permalinkRouter :: FilePath -> [(String, String)] -> FilePath -> Rules FilePath
permalinkRouter outDir extAssoc src = do
  yamlFrontmatter <- liftIO $ readYamlFrontmatter src
  permalink <- either fail return $ yamlFrontmatter ^. "permalink"
  let out = outDir </> removeLeadingSlash (Text.unpack permalink)
  let srcExt = takeExtension src
  liftIO $ print srcExt
  let indexFile = "index" <.> fromMaybe srcExt (List.lookup srcExt extAssoc)
  let outIsDir = "/" `List.isSuffixOf` out
  return $ if outIsDir then out </> indexFile else out

removeLeadingSlash :: FilePath -> FilePath
removeLeadingSlash path
  | "/" `List.isPrefixOf` path = tail path
  | otherwise = path

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
