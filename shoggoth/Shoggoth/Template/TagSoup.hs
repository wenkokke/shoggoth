module Shoggoth.Template.TagSoup
  ( stripTags,
    withUrls,
    addDefaultTableHeaderScope,
    removeFootnoteAnchorId,
    module TagSoup,
  )
where

import Shoggoth.Prelude ( Url )
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.HTML.TagSoup qualified as TagSoup

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
withUrls f = TagSoup.renderTags . map tag . TagSoup.parseTags
  where
    tag (TagSoup.TagOpen s a) = TagSoup.TagOpen s $ map attr a
    tag x = x
    attr (k, v) = (k, if k `elem` refs then f v else v)
    refs = ["src", "href", "xlink:href"]

-- | Pandoc does not include scope tags for table header elements.
addDefaultTableHeaderScope :: Text -> Text -> Text
addDefaultTableHeaderScope defaultScope = TagSoup.renderTags . map tag . TagSoup.parseTags
  where
    tag (TagSoup.TagOpen s a) | s == "th" && "scope" `notElem` map fst a = TagSoup.TagOpen s (("scope", defaultScope) : a)
    tag x = x


-- | Pandoc automatically adds anchors to footnotes, and sometimes it is useful to remove these.
removeFootnoteAnchorId :: Text  -> Text
removeFootnoteAnchorId = TagSoup.renderTags . map tag . TagSoup.parseTags
  where
    tag (TagSoup.TagOpen s a) = TagSoup.TagOpen s (filter (not . isFootnoteAnchorId) a)
    tag x = x
    isFootnoteAnchorId (k,v) = k == "id" && "fnref" `Text.isPrefixOf` v