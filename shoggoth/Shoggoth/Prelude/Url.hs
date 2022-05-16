{-# LANGUAGE OverloadedStrings #-}

module Shoggoth.Prelude.Url where

import Shoggoth.Prelude.FilePath
  ( joinPath,
    splitDirectories,
    takeDirectory, makeRelative
  )
import Data.Text (Text)
import Data.Text qualified as Text

type Url = Text

implicitIndexFile :: Url -> Url
implicitIndexFile = Text.replace "index.html" ""

-- | Make a Url relative to the site's root directory.
--
--   Adapted from hakyll's 'Hakyll.Web.Html.RelativizeUrls.relativizeUrls'
relativizeUrl :: FilePath -> FilePath -> Url -> Url
relativizeUrl outDir out url
  | "/" `Text.isPrefixOf` url && not ("//" `Text.isPrefixOf` url) = Text.pack (toRoot out) <> url
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