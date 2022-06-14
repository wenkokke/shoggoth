module Shoggoth.Template.Pandoc.Helper where

import Data.Function ((&))
import Shoggoth.Prelude.Url (Url)
import Shoggoth.TagSoup qualified as TagSoup
import Text.Pandoc.Definition (Block (..), Format (..), Inline (..), Pandoc, Attr)
import Text.Pandoc.Walk (walk)
import Data.Text (Text)

-- | Apply a function to each Url in a Pandoc document.
withUrls :: (Url -> Url) -> Pandoc -> Pandoc
withUrls f doc =
  doc
    & walk (withUrlsInline f)
    & walk (withUrlsBlock f)

-- | Apply a function to each Url in a Pandoc 'Block' element.
withUrlsBlock :: (Url -> Url) -> Block -> Block
withUrlsBlock f (RawBlock fmt@(Format "html") raw) =
  RawBlock fmt (TagSoup.withUrls f raw)
withUrlsBlock f (RawBlock fmt@(Format _) raw) =
  RawBlock fmt (TagSoup.withUrls f raw)
withUrlsBlock _f block = block

-- | Apply a function to each Url in a Pandoc 'Inline' element.
withUrlsInline :: (Url -> Url) -> Inline -> Inline
withUrlsInline f (Link attr inlines (url, title)) =
  Link attr inlines (f url, title)
withUrlsInline f (RawInline fmt@(Format "html") raw) =
  RawInline fmt (TagSoup.withUrls f raw)
withUrlsInline f (RawInline fmt@(Format _) raw) =
  RawInline fmt (TagSoup.withUrls f raw)
withUrlsInline _f inline = inline


-- | Apply a function to each Id in a Pandoc document.
withIds :: (Url -> Url) -> Pandoc -> Pandoc
withIds f doc =
  doc
    & walk (withIdsInline f)
    & walk (withIdsBlock f)

-- | Apply a function to each Id in a Pandoc 'Block' element.
withIdsBlock :: (Text -> Text) -> Block -> Block
withIdsBlock f (CodeBlock attr text) =
  CodeBlock (withIdsAttr f attr) text
withIdsBlock f (Header level attr inlines) =
  Header level (withIdsAttr f attr) inlines
withIdsBlock f (Table attr caption colspec thead tbody tfoot) =
  Table (withIdsAttr f attr) caption colspec thead tbody tfoot
withIdsBlock f (Div attr blocks) =
  Div (withIdsAttr f attr) blocks
withIdsBlock f (RawBlock fmt@(Format "html") raw) =
  RawBlock fmt (TagSoup.withIds f raw)
withIdsBlock f (RawBlock fmt@(Format _) raw) =
  RawBlock fmt (TagSoup.withIds f raw)
withIdsBlock _f block = block

-- | Apply a function to each Id in a Pandoc 'Inline' element.
withIdsInline :: (Text -> Text) -> Inline -> Inline
withIdsInline f (Code attr text) =
  Code (withIdsAttr f attr) text
withIdsInline f (Link attr inlines (url, title)) =
  Link (withIdsAttr f attr) inlines (url, title)
withIdsInline f (Image attr inlines (url, title)) =
  Image (withIdsAttr f attr) inlines (url, title)
withIdsInline f (Span attr inlines) =
  Span (withIdsAttr f attr) inlines
withIdsInline f (RawInline fmt@(Format "html") raw) =
  RawInline fmt (TagSoup.withIds f raw)
withIdsInline f (RawInline fmt@(Format _) raw) =
  RawInline fmt (TagSoup.withIds f raw)
withIdsInline _f inline = inline

-- | Apply a function to each Id in a Pandoc 'Attr' element.
withIdsAttr :: (Text -> Text) -> Attr -> Attr
withIdsAttr f (id, classes, attrs) = (f id, classes, attrs)
