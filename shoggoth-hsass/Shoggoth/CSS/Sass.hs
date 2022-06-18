module Shoggoth.CSS.Sass
  ( compileSass,
    compileSassWith
  )
where

import Shoggoth.Prelude
import Shoggoth.Prelude.ByteString qualified as BS (toText)
import Data.Bitraversable (Bitraversable (..))
import Data.Maybe (fromMaybe)
import System.Directory as System (doesFileExist, makeAbsolute, getCurrentDirectory)
import Text.Sass
import Data.Text (Text)

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
    css <- BS.toText (resultString result)
    includes <- resultIncludes result
    currentWorkingDirectory <- getCurrentDirectory
    let relativeIncludes = makeRelative currentWorkingDirectory <$> includes
    return (css, relativeIncludes)

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
