module Shoggoth.Template.Pandoc.Helper where

import Data.Function ((&))
import Shoggoth.Prelude.Url (Url)
import Shoggoth.TagSoup qualified as TagSoup
import Text.Pandoc.Definition (Block (..), Format (..), Inline (..), Pandoc, Attr)
import Text.Pandoc.Walk (Walkable (walk))
import Data.Text (Text)

-- | Apply a function to each Url in a Pandoc document.
withUrls :: (Walkable Block doc, Walkable Inline doc) => (Url -> Url) -> doc -> doc
withUrls f doc = doc & withUrlsInline f & withUrlsBlock f

-- | Apply a function to each Url in a Pandoc 'Block' element.
withUrlsBlock :: Walkable Block doc => (Url -> Url) -> doc -> doc
withUrlsBlock f = walk go
  where
    go (RawBlock fmt@(Format "html") raw) =
      RawBlock fmt (TagSoup.withUrls f raw)
    go (RawBlock fmt@(Format _) raw) =
      RawBlock fmt (TagSoup.withUrls f raw)
    go block = block

-- | Apply a function to each Url in a Pandoc 'Inline' element.
withUrlsInline :: Walkable Inline doc => (Url -> Url) -> doc -> doc
withUrlsInline f = walk go
  where
    go (Link attr inlines (url, title)) =
      Link attr inlines (f url, title)
    go (RawInline fmt@(Format "html") raw) =
      RawInline fmt (TagSoup.withUrls f raw)
    go (RawInline fmt@(Format _) raw) =
      RawInline fmt (TagSoup.withUrls f raw)
    go inline = inline


-- | Apply a function to each Id in a Pandoc document.
withIds :: (Walkable Block doc, Walkable Inline doc) => (Url -> Url) -> doc -> doc
withIds f doc = doc & withIdsInline f & withIdsBlock f

-- | Apply a function to each Id in a Pandoc 'Block' element.
withIdsBlock :: Walkable Block doc => (Text -> Text) -> doc -> doc
withIdsBlock f = walk go
  where
    go (CodeBlock attr text) =
      CodeBlock (withIdsAttr f attr) text
    go (Header level attr inlines) =
      Header level (withIdsAttr f attr) inlines
    go (Table attr caption colspec thead tbody tfoot) =
      Table (withIdsAttr f attr) caption colspec thead tbody tfoot
    go (Div attr blocks) =
      Div (withIdsAttr f attr) blocks
    go (RawBlock fmt@(Format "html") raw) =
      RawBlock fmt (TagSoup.withIds f raw)
    go (RawBlock fmt@(Format _) raw) =
      RawBlock fmt (TagSoup.withIds f raw)
    go block = block

-- | Apply a function to each Id in a Pandoc 'Inline' element.
withIdsInline :: Walkable Inline doc => (Text -> Text) -> doc -> doc
withIdsInline f = walk go
  where
    go (Code attr text) =
      Code (withIdsAttr f attr) text
    go (Link attr inlines (url, title)) =
      Link (withIdsAttr f attr) inlines (url, title)
    go (Image attr inlines (url, title)) =
      Image (withIdsAttr f attr) inlines (url, title)
    go (Span attr inlines) =
      Span (withIdsAttr f attr) inlines
    go (RawInline fmt@(Format "html") raw) =
      RawInline fmt (TagSoup.withIds f raw)
    go (RawInline fmt@(Format _) raw) =
      RawInline fmt (TagSoup.withIds f raw)
    go inline = inline

-- | Apply a function to each Id in a Pandoc 'Attr' element.
withIdsAttr :: (Text -> Text) -> Attr -> Attr
withIdsAttr f (id, classes, attrs) = (f id, classes, attrs)
