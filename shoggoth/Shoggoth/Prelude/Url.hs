module Shoggoth.Prelude.Url where

import Data.Text (Text)
import Data.Text qualified as Text
import Shoggoth.Prelude.FilePath
  ( joinPath,
    makeRelative,
    splitDirectories,
    takeDirectory, normaliseEx
  )

type Url = Text

isAbsoluteUrl :: Url -> Bool
isAbsoluteUrl url = "/" `Text.isPrefixOf` url && not ("//" `Text.isPrefixOf` url)

removeIndexHtml :: Url -> Url
removeIndexHtml = Text.replace "index.html" ""

-- | Make a Url relative to the site's root directory.
--
--   Adapted from hakyll's 'Hakyll.Web.Html.RelativizeUrls.relativizeUrls'
relativizeUrl :: FilePath -> FilePath -> Url -> Url
relativizeUrl outDir out url
  | isAbsoluteUrl url = Text.pack toRoot <> url
  | otherwise = url
  where
    toRoot :: FilePath
    toRoot
      | null directories = "."
      | otherwise = joinPath $ map (const "..") directories
      where
        relativeOut = normaliseEx (makeRelative outDir out)
        directories = filter (`notElem` [".", "/", "./"]) (splitDirectories (takeDirectory out))