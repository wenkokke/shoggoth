module Shoggoth.CSS.Sass
  ( -- * Compile Sass with hsass
    compileSass,
    compileSassWith,
    minCssImporter,
    SassOptions (..),
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

-- | Compile Sass.
compileSass :: FilePath -> Action Text
compileSass = compileSassWith def

-- | Compile Sass with options.
compileSassWith :: SassOptions -> FilePath -> Action Text
compileSassWith opts filePath = do
  (css, includes) <- liftIO $ do
    -- Compile @filePath@ from Sass/SCSS to CSS
    resultOrError <- liftIO $ compileFile filePath opts
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
