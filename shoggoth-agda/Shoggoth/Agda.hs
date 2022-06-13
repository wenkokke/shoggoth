{-# LANGUAGE CPP #-}

module Shoggoth.Agda
  ( compileTo,
    Format (..),
    Library (..),
    makeAgdaLinkFixer,
    makeLocalLinkFixer,
    makeLibraryLinkFixer,
    makeBuiltinLinkFixer,
    getStandardLibraryIO,
    AgdaFileInfo (..),
    ModuleName,
    resolveFileInfo,
    isAgdaFile,
    AgdaException (..),
    makeVersionOracle,
    getVersion,
    AgdaStandardLibraryException (..),
    makeStandardLibraryOracle,
    getStandardLibrary,
    AgdaSoup,
    runAgdaSoup,
    qualifyIdSoup,
    AgdaSoupException (..)
  )
where

#if installAgda
import Agda.Main qualified as Agda
import Agda.Version qualified as Agda
import Control.Exception (handle, throwIO)
import System.Environment (withArgs, withProgName)
import System.Exit (ExitCode (..))
#endif

import Control.Exception (Exception, throwIO)
import Control.Monad (MonadPlus (mzero), forM, join, msum)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid (Endo (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.ICU qualified as RE
import Data.Text.ICU.Replace qualified as RE
import Data.Text.IO qualified as Text
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Shoggoth.Configuration (getCacheDirectory)
import Shoggoth.Prelude
import Shoggoth.Routing
import System.Directory qualified as System (doesFileExist)
import Control.Exception (Exception, throw)
import Control.Monad (when)
import Control.Monad.Trans.State.Strict qualified as State
  ( State,
    evalState,
    get,
    put,
    modify
  )
import Shoggoth.TagSoup
  ( Attribute,
    Tag (TagOpen, TagPosition),
    isTagCloseName,
    isTagOpenName,
    parseTags,
    renderTags,
    Row,
    Column
  )

-- Agda version oracle

newtype AgdaVersionQuery = AgdaVersionQuery ()
  deriving (Show, Typeable, Eq, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult AgdaVersionQuery = Text

data AgdaException
    = AgdaCannotParseVersionLine Text
    deriving (Show, Typeable)
    deriving anyclass (Exception)

makeVersionOracle :: Rules ()
makeVersionOracle = do
  addOracle $ \AgdaVersionQuery{} -> getVersionAction
  return ()



#if installAgda
getVersionAction :: Action Text
getVersionAction = do
  putInfo $ "Using Agda version " <> Agda.version
  return $ Text.pack Agda.version
#else
getVersionAction :: Action Text
getVersionAction = do
  Stdout versionLineString <- command [] "agda" ["--version"]
  let versionLine = Text.strip $ Text.pack versionLineString
  case Text.stripPrefix "Agda version " versionLine of
    Just agdaVersion -> do
      putInfo $ "Using Agda version " <> Text.unpack agdaVersion
      return agdaVersion
    Nothing -> do
      liftIO $ throwIO $ AgdaCannotParseVersionLine versionLine
#endif

getVersion :: Action Text
getVersion = askOracle $ AgdaVersionQuery ()

-- Compiling Agda

compileTo :: Format -> [Library] -> FilePath -> FilePath -> Action ()
compileTo fmt libs outDir src = do
  runAgdaWith $
    concat
      [ ["--verbose=0"],
        formatArgs fmt outDir,
        libraryArgs libs,
        [src]
      ]

#if installAgda
runAgdaWith :: [String] -> Action ()
runAgdaWith args =
  liftIO (withProgName "agda" (withArgs args (handle handleExitSuccess (Agda.runAgda []))))
  where
    handleExitSuccess :: ExitCode -> IO ()
    handleExitSuccess ExitSuccess = return ()
    handleExitSuccess exitCode    = throwIO exitCode
#else
runAgdaWith :: [String] -> Action ()
runAgdaWith args = command_ [] "agda" args
#endif

type ModuleName = Text

data Format
  = Html
  | LaTeX
  deriving (Show, Typeable, Eq, Generic)
  deriving anyclass (Hashable, Binary, NFData)

data Library = Library
  { libraryName :: Text,
    -- | The directory which contains the .agda-lib file.
    libraryRoot :: FilePath,
    -- | A series of paths in which to search for library files, relative to the directory in which the library file resides.
    includePaths :: [FilePath],
    -- | A canonical URL to which to redirect links.
    canonicalBaseUrl :: Url
  }
  deriving (Show, Typeable, Eq, Generic)
  deriving anyclass (Hashable, Binary, NFData)

fullIncludePaths :: Library -> [FilePath]
fullIncludePaths lib@Library {..} = [normaliseEx (libraryRoot </> includePath) | includePath <- includePaths]

libraryArgs :: [Library] -> [String]
libraryArgs libs =
  mconcat
    [ ["--no-libraries"],
      [ "--include-path=" <> includePath
        | lib <- libs,
          includePath <- fullIncludePaths lib
      ]
    ]

formatArgs :: Format -> FilePath -> [String]
formatArgs Html = htmlArgs
formatArgs LaTeX = latexArgs

htmlArgs :: FilePath -> [String]
htmlArgs outDir = ["--html", "--html-dir=" <> outDir, "--html-highlight=code"]

latexArgs :: FilePath -> [String]
latexArgs outDir = ["--latex", "--latex-dir=" <> outDir]

--------------------------------------------------------------------------------
-- Construct an Agda link fixer
--------------------------------------------------------------------------------

makeAgdaLinkFixer ::
  (?routingTable :: RoutingTable) =>
  (Text -> Text) ->
  Maybe Library ->
  [Library] ->
  [Library] ->
  Action (Url -> Url)
makeAgdaLinkFixer qualifyId standardLibrary localLibraries otherLibraries = do
  let maybeBuiltinLinkFixer = makeBuiltinLinkFixer <$> standardLibrary
  maybeStandardLibraryLinkFixer <- traverse makeLibraryLinkFixer standardLibrary
  localLinkFixers <- traverse makeLocalLinkFixer localLibraries
  otherLinkFixers <- traverse makeLibraryLinkFixer otherLibraries
  let linkFixers =
        [ maybeToList maybeBuiltinLinkFixer,
          maybeToList maybeStandardLibraryLinkFixer,
          otherLinkFixers,
          localLinkFixers
        ]
  return . appEndo . mconcat . fmap Endo . concat $ linkFixers

--------------------------------------------------------------------------------
-- Fix references to local Agda modules using a routing table
--------------------------------------------------------------------------------

-- | Create a function to fix URL references to local modules
makeLocalLinkFixer ::
  (?routingTable :: RoutingTable) =>
  Library ->
  Action (Url -> Url)
makeLocalLinkFixer library@Library {..} = do
  files <- getAgdaFilesInLibrary library

  moduleRoutes <- forM files $ \(includePath, file) -> do
    let src = libraryRoot </> includePath </> file
    let moduleName = modulePathToName file
    url <- routeUrl src
    return (moduleName, url)
  let moduleRoutingTable = Map.fromList moduleRoutes

  return $ \url -> fromMaybe url $ do
    let (oldUrl, anchor) = Text.breakOn "#" url
    let moduleName = Text.replace ".html" "" oldUrl
    newUrl <- Map.lookup moduleName moduleRoutingTable
    return $ newUrl <> qualifyName library moduleName anchor

--------------------------------------------------------------------------------
-- Fix references to an external library with a canonical URL
--------------------------------------------------------------------------------

makeLibraryLinkFixer :: MonadIO m => Library -> m (Url -> Url)
makeLibraryLinkFixer lib@Library {..} = do
  regex <- reLibraryLink lib
  return $ RE.replaceAll regex (RE.rtext canonicalBaseUrl <> "/$1.html$2")

reLibraryLink :: MonadIO m => Library -> m RE.Regex
reLibraryLink lib = do
  modNames <- getAgdaModulesInLibrary lib
  let modPatns = Text.replace "." "\\." <$> modNames
  let modPatn = Text.concat . List.intersperse "|" $ modPatns
  let hrefPatn = "(" <> modPatn <> ")\\.html(#[^\"^']+)?"
  return (RE.regex [] hrefPatn)

--------------------------------------------------------------------------------
-- Fix references to the Agda builtin modules
--------------------------------------------------------------------------------

makeBuiltinLinkFixer :: Library -> (Url -> Url)
makeBuiltinLinkFixer Library {..} = do
  RE.replaceAll reAgdaBuiltinLink (RE.rtext canonicalBaseUrl <> "/$1.html$2")

-- | An ICU regular expression which matches links to the Agda builtin modules.
reAgdaBuiltinLink :: RE.Regex
reAgdaBuiltinLink = RE.regex [] "(Agda\\.[A-Za-z\\.]+)\\.html(#[^\"^']+)?"

--------------------------------------------------------------------------------
-- A library instance for the standard library
--------------------------------------------------------------------------------

newtype AgdaStandardLibraryQuery = AgdaStandardLibraryQuery ()
  deriving (Show, Typeable, Eq, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult AgdaStandardLibraryQuery = Library

data AgdaStandardLibraryException
    = AgdaStandardLibraryNotFound FilePath
    | AgdaStandardLibraryCannotParseVersionLine FilePath Text
    deriving (Show, Typeable)
    deriving anyclass (Exception)

makeStandardLibraryOracle :: FilePath -> Rules ()
makeStandardLibraryOracle libraryRoot = do
  addOracle $ \AgdaStandardLibraryQuery{} -> do
    standardLibraryVersion <- getStandardLibraryVersionIO libraryRoot
    putInfo $ "Using Agda standard library version " <> Text.unpack standardLibraryVersion
    let includePaths = ["src"]
    let canonicalBaseUrl = makeStandardLibraryCanonicalBaseUrl standardLibraryVersion
    return Library {libraryName = "standard-library", ..}
  return ()

getStandardLibrary :: Action Library
getStandardLibrary = askOracle $ AgdaStandardLibraryQuery ()

getStandardLibraryIO :: MonadIO m => FilePath -> m Library
getStandardLibraryIO libraryRoot = do
  let includePaths = ["src"]
  standardLibraryVersion <- getStandardLibraryVersionIO libraryRoot
  let canonicalBaseUrl = makeStandardLibraryCanonicalBaseUrl standardLibraryVersion
  return Library {libraryName = "standard-library", ..}

-- | Get a URL to the standard library documentation.
makeStandardLibraryCanonicalBaseUrl :: Text -> Text
makeStandardLibraryCanonicalBaseUrl standardLibraryVersion =
  "https://agda.github.io/agda-stdlib/" <> standardLibraryVersion


-- | Get the standard library version.
getStandardLibraryVersionIO :: MonadIO m => FilePath -> m Text
getStandardLibraryVersionIO dir = liftIO $ do
  --
  -- NOTE: Version detection depends on the fact that the standard library
  --       maintains a CHANGELOG.md file which always opens with its version.
  --
  let changelog = dir </> "CHANGELOG.md"
  correct <- System.doesFileExist changelog
  if not correct
    then throwIO $ AgdaStandardLibraryNotFound changelog
    else do
      changelogContents <- Text.readFile changelog
      let versionLine = head (Text.lines changelogContents)
      case Text.stripPrefix "Version " versionLine of
        Nothing -> throwIO $ AgdaStandardLibraryCannotParseVersionLine changelog versionLine
        Just standardLibraryVersion -> return $ "v" <> Text.strip standardLibraryVersion

--------------------------------------------------------------------------------
-- Guess to which file Agda writes HTML and LaTeX output
--------------------------------------------------------------------------------

data AgdaFileInfo = AgdaFileInfo
  { -- | The Agda library to which this file belongs.
    library :: Library,
    -- | The include path used within the library.
    libraryIncludePath :: FilePath,
    -- | The module path used within that include path.
    modulePath :: FilePath,
    -- | The Agda module name.
    moduleName :: ModuleName,
    -- | The output filename for LaTeX.
    outputFileForLaTeX :: FilePath,
    -- | The output filename for Html.
    outputFileForHtml :: FilePath
  }
  deriving (Show, Typeable, Eq, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Convert a filepath to a module name.
modulePathToName :: FilePath -> Text
modulePathToName path = Text.map sepToDot (Text.pack $ dropExtensions path)
  where
    sepToDot c = if isPathSeparator c then '.' else c

-- | Guess the module path based on the filename and the library.
resolveFileInfo :: MonadError String m => [Library] -> FilePath -> m AgdaFileInfo
resolveFileInfo libs src = fromCandidates (resolveFileInfoForLibraries libs)
  where
    resolveFileInfoForLibraries :: MonadPlus m => [Library] -> m AgdaFileInfo
    resolveFileInfoForLibraries libs =
      msum [resolveFileInfoForLibrary lib | lib <- libs]
      where
        resolveFileInfoForLibrary :: MonadPlus m => Library -> m AgdaFileInfo
        resolveFileInfoForLibrary lib =
          msum [resolveFileInfoForIncludePath (libraryRoot lib) includePath | includePath <- includePaths lib]
          where
            resolveFileInfoForIncludePath :: MonadPlus m => FilePath -> FilePath -> m AgdaFileInfo
            resolveFileInfoForIncludePath libraryRoot includePath
              | src `inDirectory` fullIncludePath =
                let modulePath         = makeRelative fullIncludePath src
                    moduleName         = modulePathToName modulePath
                    outputFileForHtml  = Text.unpack moduleName <.> "md"
                    outputFileForLaTex = replaceExtensions modulePath "tex"
                 in return (AgdaFileInfo lib includePath modulePath moduleName outputFileForLaTex outputFileForHtml)
              | otherwise = mzero
              where
                fullIncludePath = normaliseEx (libraryRoot </> includePath)

    fromCandidates :: (MonadError String m, Show a) => [a] -> m a
    fromCandidates [] = throwError $ "Could not find candidate for " <> src
    fromCandidates [fileInfo] = return fileInfo
    fromCandidates candidates = throwError $ "Multiple candidates for " <> src <> ": " <> show candidates

-- | Check whether the first argument is contained within the second argument.
--   Assumes that both paths canonical and are either absolute or relative to the same directory.
inDirectory :: FilePath -> FilePath -> Bool
inDirectory file dir =
  splitDirectories (normaliseEx dir) `List.isPrefixOf` splitDirectories (normaliseEx file)


--------------------------------------------------------------------------------
-- Fix Agda IDs using TagSoup
--------------------------------------------------------------------------------

data AgdaSoupState = AgdaSoupState
  { openPreTags :: [Tag Text],
    tagPosition :: (Row, Column)
  }

emptyAgdaSoupState :: AgdaSoupState
emptyAgdaSoupState = AgdaSoupState
  { openPreTags = [],
    tagPosition = (0, 0)
  }

openPreTag :: Tag Text -> AgdaSoupState -> AgdaSoupState
openPreTag preTagOpen st@AgdaSoupState {openPreTags} = do
  st {openPreTags = preTagOpen : openPreTags}

closePreTag :: Tag Text -> AgdaSoupState -> AgdaSoupState
closePreTag _preTagClose st@AgdaSoupState {openPreTags, tagPosition} =
  case openPreTags of
    [] -> throw $ uncurry ExtraPreClose tagPosition
    (_firstPreTag : otherPreTags) -> st {openPreTags = otherPreTags}

data AgdaSoupException
  = ExtraPreClose !Row !Column
  deriving (Show)
  deriving anyclass (Exception)

newtype AgdaSoup a = AgdaSoup (State.State AgdaSoupState a)
  deriving newtype (Functor, Applicative, Monad)

qualifyName :: Library -> ModuleName -> Text -> Text
qualifyName Library{libraryName} moduleName name = libraryName <> ":" <> moduleName <> "." <> name

qualifyIdSoup :: AgdaFileInfo -> Tag Text -> AgdaSoup (Tag Text)
qualifyIdSoup AgdaFileInfo{library, moduleName} = mapIdSoup (qualifyName library moduleName)

runAgdaSoup :: AgdaSoup a -> a
runAgdaSoup (AgdaSoup m) = State.evalState m emptyAgdaSoupState

mapIdSoup :: (Text -> Text) -> Tag Text -> AgdaSoup (Tag Text)
mapIdSoup f tag@(TagPosition r c) = AgdaSoup $ do
    State.modify (\st -> st { tagPosition = (r, c) })
    return tag
mapIdSoup f tag
  | isPreOpen tag = AgdaSoup $ do
    State.modify (openPreTag tag)
    return tag
  | isPreClose tag = AgdaSoup $ do
    State.modify (closePreTag tag)
    return tag
  | otherwise = AgdaSoup $ do
    cond <- inAgdaPre
    return $ if cond then mapIdTag f tag else tag

mapIdTag :: (Text -> Text) -> Tag Text -> Tag Text
mapIdTag f (TagOpen name attrs) =
  TagOpen
    name
    [ if key == "id" then (key, f value) else attr
      | attr@(key, value) <- attrs
    ]
mapIdTag f tag = tag

hasAgdaClass :: Tag Text -> Bool
hasAgdaClass = hasAttribute ("class", "Agda")
  where
    hasAttribute :: Attribute Text -> Tag Text -> Bool
    hasAttribute attr (TagOpen tag attrs) = attr `elem` attrs
    hasAttribute _ _ = False

isPreOpen :: Tag Text -> Bool
isPreOpen = isTagOpenName "pre"

isPreClose :: Tag Text -> Bool
isPreClose = isTagCloseName "pre"

inAgdaPre :: State.State AgdaSoupState Bool
inAgdaPre = do
  st@AgdaSoupState {openPreTags} <- State.get
  return (any hasAgdaClass openPreTags)


--------------------------------------------------------------------------------
-- Helper functions to get files in an Agda library
--------------------------------------------------------------------------------

-- | Get module names for all Agda modules in a library.
getAgdaModulesInLibrary :: MonadIO m => Library -> m [ModuleName]
getAgdaModulesInLibrary lib = do
  files <- getAgdaFilesInLibrary lib
  return [modulePathToName file | (includePath, file) <- files]

-- | Get file paths for each Agda file in the library, together with its include directory.
getAgdaFilesInLibrary :: MonadIO m => Library -> m [(FilePath, FilePath)]
getAgdaFilesInLibrary lib@Library {..} = do
  filesByIncludePath <- forM includePaths $ \includePath -> do
    files <- getAgdaFilesInDirectory (libraryRoot </> includePath)
    return $ (includePath,) <$> files
  return $ concat filesByIncludePath

-- | Get file paths for each Agda file in the directory.
getAgdaFilesInDirectory :: MonadIO m => FilePath -> m [FilePath]
getAgdaFilesInDirectory dir =
  liftIO $
    getDirectoryFilesIO dir ["//*.agda", "//*.lagda", "//*.lagda.md", "//*.lagda.org", "//*.lagda.rst", "//*.lagda.tex"]

-- | Check if the path points to an Agda or literate Agda file.
isAgdaFile :: FilePath -> Bool
isAgdaFile src = any (?== src) ["//*.agda", "//*.lagda", "//*.lagda.md", "//*.lagda.org", "//*.lagda.rst", "//*.lagda.tex"]
