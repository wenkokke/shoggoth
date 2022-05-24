module Shoggoth.Template.Pandoc.Url where

import Data.Function ((&))
import Shoggoth.Prelude.Url (Url)
import Shoggoth.TagSoup qualified as TagSoup
import Text.Pandoc.Definition (Block (..), Format (..), Inline (..), Pandoc)
import Text.Pandoc.Walk (walk)

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