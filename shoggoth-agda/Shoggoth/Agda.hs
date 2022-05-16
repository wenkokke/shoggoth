{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Shoggoth.Agda
  ( compileTo,
    Format (..),
    Library (..),
    makeLocalLinkFixer,
    makeLibraryLinkFixer,
    makeBuiltinLinkFixer,
    getStandardLibrary,
    htmlOutputPath,
    latexOutputPath,
    isAgdaFile,
  )
where

#if installAgda
import Agda.Main qualified as Agda
import Control.Exception (handle, throwIO)
import System.Environment (withArgs, withProgName)
import System.Exit (ExitCode (..))
#endif

import Shoggoth.Prelude
import Shoggoth.Routing
import Control.Monad (MonadPlus (mzero), forM, join, msum)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (MonadError (throwError))
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.ICU qualified as RE
import Data.Text.ICU.Replace qualified as RE
import Data.Text.IO qualified as Text
import System.Directory qualified as System (doesFileExist)

compileTo :: Format -> [Library] -> FilePath -> FilePath -> Action ()
compileTo fmt libs outDir src = do
  need [src]
  runAgdaWith $ concat
    [ ["--verbose=0"],
      formatArgs fmt outDir,
      libraryArgs libs,
      [ src ]
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
  deriving (Eq, Ord, Show)

data Library = Library
  { -- | The directory which contains the .agda-lib file.
    libraryRoot :: FilePath,
    -- | A series of paths in which to search for library files, relative to the directory in which the library file resides.
    includePaths :: [FilePath],
    -- | A canonical URL to which to redirect links.
    canonicalBaseUrl :: Url
  }
  deriving (Eq, Show)

fullIncludePaths :: Library -> [FilePath]
fullIncludePaths lib@Library {..} = [libraryRoot </> includePath | includePath <- includePaths]

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
-- Fix references to local Agda modules using a routing table
--------------------------------------------------------------------------------

-- | Create a function to fix URL references to local modules
makeLocalLinkFixer ::
  (?routingTable :: RoutingTable) =>
  Library ->
  Action (Url -> Url)
makeLocalLinkFixer lib@Library {..} = do
  files <- getAgdaFilesInLibrary lib

  moduleRoutes <- forM files $ \(includePath, file) -> do
    let src = libraryRoot </> includePath </> file
    let moduleName = modulePathToName file
    out <- route src
    return (moduleName, Text.pack out)
  let moduleRoutingTable = Map.fromList moduleRoutes

  return $ \url -> fromMaybe url $ do
    let (oldUrl, anchor) = Text.breakOn "#" url
    let moduleName = Text.replace ".html" "" oldUrl
    newUrl <- Map.lookup moduleName moduleRoutingTable
    return $ newUrl <> anchor

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
-- Blag a library instance for the standard library
--------------------------------------------------------------------------------

getStandardLibrary :: MonadIO m => FilePath -> m Library
getStandardLibrary libraryRoot = do
  let includePaths = ["src"]
  canonicalBaseUrl <- getStandardLibraryCanonicalBaseUrl libraryRoot
  return Library {..}

-- | Get a URL to the standard library documentation.
getStandardLibraryCanonicalBaseUrl :: MonadIO m => FilePath -> m Text
getStandardLibraryCanonicalBaseUrl dir = do
  ver <- getStandardLibraryVersion dir
  return $ "https://agda.github.io/agda-stdlib/" <> ver

-- | Get the standard library version.
getStandardLibraryVersion :: MonadIO m => FilePath -> m Text
getStandardLibraryVersion dir = liftIO $ do
  --
  -- NOTE: Version detection depends on the fact that the standard library
  --       maintains a CHANGELOG.md file which always opens with its version.
  --
  let changelog = dir </> "CHANGELOG.md"
  correct <- System.doesFileExist changelog
  if not correct
    then error $ "Could not find " <> changelog
    else do
      changelogContents <- Text.readFile changelog
      let verLine = head (Text.lines changelogContents)
      ver <-
        maybe (fail $ "Cannot read version from " <> changelog) return $
          Text.stripPrefix "Version " verLine
      return $ "v" <> Text.strip ver

--------------------------------------------------------------------------------
-- Guess to which file Agda writes HTML and LaTeX output
--------------------------------------------------------------------------------

htmlOutputPath :: MonadError String m => FilePath -> [Library] -> FilePath -> m FilePath
htmlOutputPath = resolveOutputPath Html

latexOutputPath :: MonadError String m => FilePath -> [Library] -> FilePath -> m FilePath
latexOutputPath = resolveOutputPath LaTeX

-- | Guess the path to which Agda writes the relevant output file.
resolveOutputPath :: MonadError String m => Format -> FilePath -> [Library] -> FilePath -> m FilePath
resolveOutputPath format outDir libs inputFile = do
  (lib, modulePath, moduleName) <- resolveModulePath libs inputFile
  return $
    case format of
      Html -> outDir </> Text.unpack moduleName <.> "md"
      LaTeX -> outDir </> replaceExtensions modulePath "tex"

-- | Convert a filepath to a module name.
modulePathToName :: FilePath -> Text
modulePathToName path = Text.map sepToDot (Text.pack $ dropExtensions path)
  where
    sepToDot c = if isPathSeparator c then '.' else c

-- | Guess the module path based on the filename and the library.
resolveModulePath :: MonadError String m => [Library] -> FilePath -> m (Library, FilePath, ModuleName)
resolveModulePath libs src = fromCandidates (resolveModuleForLibraries libs)
  where
    resolveModuleForLibraries :: MonadPlus m => [Library] -> m (Library, FilePath, ModuleName)
    resolveModuleForLibraries libs =
      msum [resolveModuleForLibrary lib | lib <- libs]
      where
        resolveModuleForLibrary :: MonadPlus m => Library -> m (Library, FilePath, ModuleName)
        resolveModuleForLibrary lib =
          msum [resolveModuleForIncludePath includePath | includePath <- fullIncludePaths lib]
          where
            resolveModuleForIncludePath :: MonadPlus m => FilePath -> m (Library, FilePath, ModuleName)
            resolveModuleForIncludePath fullIncludePath
              | src `inDirectory` fullIncludePath =
                let modulePath = makeRelative fullIncludePath src
                    moduleName = modulePathToName modulePath
                 in return (lib, modulePath, moduleName)
              | otherwise = mzero

    fromCandidates :: (MonadError String m, Show a) => [a] -> m a
    fromCandidates [] = throwError $ "Could not find candidate for " <> src
    fromCandidates [modulePath] = return modulePath
    fromCandidates candidates = throwError $ "Multiple candidates for " <> src <> ": " <> show candidates

-- | Check whether the first argument is contained within the second argument.
--   Assumes that both paths canonical and are either absolute or relative to the same directory.
inDirectory :: FilePath -> FilePath -> Bool
inDirectory file dir =
  splitDirectories (normaliseEx dir) `List.isPrefixOf` splitDirectories (normaliseEx file)

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

