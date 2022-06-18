module Shoggoth.CSS.Sass
  ( compileSass,
    compileSassWith,
  )
where

import Data.Bitraversable (Bitraversable (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Shoggoth.Prelude
import System.Directory as System (doesFileExist, getCurrentDirectory, makeAbsolute)
import Text.Sass

-- * Sass

-- | Compile Sass.
compileSass :: FilePath -> Action Text
compileSass = compileSassWith []

-- | Compile Sass with options.
compileSassWith :: [FilePath] -> FilePath -> Action Text
compileSassWith loadPaths filePath = do
  (css, includes) <- liftIO $ do
    -- Compile @filePath@ from Sass/SCSS to CSS
    resultOrError <- liftIO $ compileFile filePath (sassOptions loadPaths)
    resultOrErrorMsg <- liftIO $ bitraverse errorMessage return resultOrError
    result <- liftIO (liftEither id resultOrErrorMsg)
    -- Extract generated CSS source and included files
    let cssText = Text.decodeUtf8 (resultString result)
    includes <- resultIncludes result
    currentWorkingDirectory <- getCurrentDirectory
    let relativeIncludes = makeRelative currentWorkingDirectory <$> includes
    return (cssText, relativeIncludes)

  -- Inform Shake of the dependencies used during compilation
  trackRead includes

  return css

sassOptions :: [FilePath] -> SassOptions
sassOptions [] = def
sassOptions loadPaths =
  def
    { sassIncludePaths = Just loadPaths,
      sassImporters = Just [minCssImporter loadPath 1 | loadPath <- loadPaths]
    }

minCssImporter :: FilePath -> Double -> SassImporter
minCssImporter includePath priority =
  SassImporter
    { importerPriority = priority,
      importerFunction = \importPath _ -> do
        let minCssPath = includePath </> importPath -<.> "min.css"
        minCssExists <- System.doesFileExist minCssPath
        if minCssExists
          then do
            minCssSource <- readFile minCssPath
            return [SassImport (Just minCssPath) Nothing (Just minCssSource) Nothing]
          else do
            return []
    }
