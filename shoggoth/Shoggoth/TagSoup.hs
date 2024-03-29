module Shoggoth.TagSoup
  ( stripTags,
    withUrls,
    mapUrls,
    withIds,
    mapIds,
    addDefaultTableHeaderScope,
    removeFootnoteAnchorId,
    removeSelfClosingCloseTags,
    removeIndexHtml,
    relativizeUrl,
    module TagSoup,
  )
where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Shoggoth.Prelude (Url)
import Shoggoth.Prelude.Url qualified as Url
import Text.HTML.TagSoup as TagSoup
  ( Attribute,
    Column,
    ParseOptions (..),
    RenderOptions (..),
    Row,
    Tag (..),
    TagRep (..),
    canonicalizeTags,
    escapeHTML,
    fromAttrib,
    fromTagText,
    innerText,
    isTagClose,
    isTagCloseName,
    isTagComment,
    isTagOpen,
    isTagOpenName,
    isTagPosition,
    isTagText,
    isTagWarning,
    maybeTagText,
    maybeTagWarning,
    parseOptions,
    parseOptionsEntities,
    parseOptionsFast,
    parseTags,
    parseTagsOptions,
    partitions,
    renderOptions,
    renderTags,
    renderTagsOptions,
    sections,
    (~/=),
    (~==),
  )

-- Strip HTML.
stripTags :: Text -> Text
stripTags = mconcat . mapMaybe tag . TagSoup.parseTags
  where
    tag (TagSoup.TagText text) = Just text
    tag _ = Nothing

-- | Apply a function to each Url in a raw HTML document.
--
--   Adapted from hakyll's 'Hakyll.Web.Html.withUrls'
withUrls :: (Url -> Url) -> Text -> Text
withUrls f = TagSoup.renderTags . map (mapUrls f) . TagSoup.parseTags

mapUrls :: (Url -> Url) -> TagSoup.Tag Text -> TagSoup.Tag Text
mapUrls f (TagSoup.TagOpen s a) = TagSoup.TagOpen s $ map attr a
  where
    attr (k, v) = (k, if k `elem` refs then f v else v)
    refs = ["src", "href", "xlink:href"]
mapUrls f tag = tag

withIds :: (Text -> Text) -> Text -> Text
withIds f = TagSoup.renderTags . map (mapIds f) . TagSoup.parseTags

mapIds :: (Text -> Text) -> TagSoup.Tag Text -> TagSoup.Tag Text
mapIds f (TagSoup.TagOpen name attrs) =
  TagSoup.TagOpen
    name
    [ if key == "id" then (key, f value) else attr
      | attr@(key, value) <- attrs
    ]
mapIds f tag = tag

-- | Pandoc does not include scope tags for table header elements.
addDefaultTableHeaderScope :: Text -> Text -> Text
addDefaultTableHeaderScope defaultScope = TagSoup.renderTags . map tag . TagSoup.parseTags
  where
    tag (TagSoup.TagOpen s a) | s == "th" && "scope" `notElem` map fst a = TagSoup.TagOpen s (("scope", defaultScope) : a)
    tag x = x

-- | Pandoc automatically adds anchors to footnotes, and sometimes it is useful to remove these.
removeFootnoteAnchorId :: Text -> Text
removeFootnoteAnchorId = TagSoup.renderTags . map tag . TagSoup.parseTags
  where
    tag (TagSoup.TagOpen s a) = TagSoup.TagOpen s (filter (not . isFootnoteAnchorId) a)
    tag x = x
    isFootnoteAnchorId (k, v) = k == "id" && "fnref" `Text.isPrefixOf` v

-- | Remove closing tags for self-closing tags such as '<img>' and '<input>'.
removeSelfClosingCloseTags :: Text -> Text
removeSelfClosingCloseTags =
  TagSoup.renderTags . filter (not . isSelfClosingCloseTag) . TagSoup.parseTags
  where
    isSelfClosingCloseTag (TagSoup.TagClose s) = s `elem` selfClosingTags
    isSelfClosingCloseTag t = False

    selfClosingTags :: [Text]
    selfClosingTags =
      [ "area",
        "base",
        "br",
        "col",
        "embed",
        "hr",
        "img",
        "input",
        "link",
        "meta",
        "param",
        "source",
        "track",
        "wbr",
        "command",
        "keygen",
        "menuitem"
      ]

removeIndexHtml :: Text -> Text
removeIndexHtml = withUrls Url.removeIndexHtml

relativizeUrl :: FilePath -> FilePath -> Text -> Text
relativizeUrl outDir out = withUrls (Url.relativizeUrl outDir out)
