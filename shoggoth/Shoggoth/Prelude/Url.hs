module Shoggoth.Prelude.Url where

import Data.Text (Text)
import Data.Text qualified as Text
import Shoggoth.Prelude.FilePath
  ( joinPath,
    makeRelative,
    splitDirectories,
    takeDirectory
  )

type Url = Text

isAbsolute :: Url -> Bool
isAbsolute url = "/" `Text.isPrefixOf` url && not ("//" `Text.isPrefixOf` url)

removeIndexHtml :: Url -> Url
removeIndexHtml = Text.replace "index.html" ""

-- | Make a Url relative to the site's root directory.
--
--   Adapted from hakyll's 'Hakyll.Web.Html.RelativizeUrls.relativizeUrls'
relativizeUrl :: FilePath -> FilePath -> Url -> Url
relativizeUrl outDir out url
  | isAbsolute url = Text.pack (toRoot out) <> url
  | otherwise = url
  where
    toRoot :: FilePath -> FilePath
    toRoot =
      joinPath
        . map (const "..")
        . filter (`notElem` [".", "/", "./"])
        . splitDirectories
        . takeDirectory
        . makeRelative outDir