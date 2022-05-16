module Shoggoth.Template.Pandoc.Citeproc
  ( module Citeproc,
    PandocCitationMode,
    pattern PandocAuthorInText,
    pattern PandocSuppressAuthor,
    pattern PandocNormalCitation,
    PandocCitation,
    pattern PandocCitation,
    pandocCitationId,
    pandocCitationPrefix,
    pandocCitationSuffix,
    pandocCitationMode,
    pandocCitationNoteNum,
    pandocCitationHash,
    getCitations,
    getCitationsFromReferences,
    getItemIds,
    insertCitations,
  )
where

import Citeproc
import Control.Monad.State qualified as State
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Text.Pandoc qualified as PandocRenamed (Citation (..), CitationMode (..))
import Text.Pandoc.Builder (Blocks, Inlines)
import Text.Pandoc.Builder qualified as Builder
import Text.Pandoc.Citeproc as Citeproc (getReferences, processCitations)
import Text.Pandoc.Definition (Inline (Cite), Pandoc)
import Text.Pandoc.Walk (Walkable (..))

type PandocCitationMode = PandocRenamed.CitationMode

pattern PandocAuthorInText :: PandocCitationMode
pattern PandocAuthorInText = PandocRenamed.AuthorInText

pattern PandocSuppressAuthor :: PandocCitationMode
pattern PandocSuppressAuthor = PandocRenamed.SuppressAuthor

pattern PandocNormalCitation :: PandocCitationMode
pattern PandocNormalCitation = PandocRenamed.NormalCitation

{-# COMPLETE PandocAuthorInText, PandocSuppressAuthor, PandocNormalCitation #-}

type PandocCitation = PandocRenamed.Citation

pattern PandocCitation ::
  Text ->
  [Inline] ->
  [Inline] ->
  PandocCitationMode ->
  Int ->
  Int ->
  PandocCitation
pattern PandocCitation {pandocCitationId, pandocCitationPrefix, pandocCitationSuffix, pandocCitationMode, pandocCitationNoteNum, pandocCitationHash} =
  PandocRenamed.Citation pandocCitationId pandocCitationPrefix pandocCitationSuffix pandocCitationMode pandocCitationNoteNum pandocCitationHash

{-# COMPLETE PandocCitation #-}

getCitationsFromReferences :: [Reference Inlines] -> [Citation Inlines]
getCitationsFromReferences = getCitations . Builder.doc . Foldable.foldMap refToBlocks
  where
    refToBlocks :: Reference Inlines -> Blocks
    refToBlocks Reference {..} = Builder.para $ Foldable.fold inlines
      where
        inlines = mapMaybe fromFancyVal (Map.elems referenceVariables)

        fromFancyVal (FancyVal inlines) = Just inlines
        fromFancyVal _ = Nothing

insertCitations :: Walkable Inline a => Map.Map [ItemId] Inlines -> a -> a
insertCitations citsByItemIds = walk insertCitation
  where
    insertCitation :: Inline -> Inline
    insertCitation cit@(Cite cs _ils) = do
      let itemIds = map (ItemId . pandocCitationId) cs
      case Map.lookup itemIds citsByItemIds of
        Nothing -> cit
        Just ils -> Cite cs (Builder.toList ils)
    insertCitation i = i

getItemIds :: Citation Inlines -> [ItemId]
getItemIds Citation{..} = map citationItemId citationItems

getCitations :: Walkable Inline a => a -> [Citation Inlines]
getCitations a = Foldable.toList (query getCitation a)
  where
    getCitation :: Inline -> Seq.Seq (Citation Inlines)
    getCitation (Cite cs _fallback) = Seq.singleton (toCitation cs)
    getCitation _ = mempty

-- | Convert a 'PandocCitation' to a Citeproc 'Citation'.
--
--   Caveats:
--   - Ignores citationItemLabel and citationItemLocator
toCitation :: [PandocCitation] -> Citation Inlines
toCitation cs =
  Citation
    { citationId = Nothing,
      citationNoteNumber = pandocCitationNoteNum <$> listToMaybe cs,
      citationItems = concatMap toCitationItem cs
    }
  where
    setCitationMode :: PandocCitationMode -> CitationItem a -> [CitationItem a]
    setCitationMode pandocCitationMode cit@CitationItem {..} = case pandocCitationMode of
      PandocAuthorInText ->
        [ cit {citationItemType = AuthorOnly, citationItemSuffix = Nothing},
          cit {citationItemType = SuppressAuthor, citationItemPrefix = Nothing}
        ]
      PandocSuppressAuthor -> [cit {citationItemType = SuppressAuthor}]
      PandocNormalCitation -> [cit]

    toCitationItem :: PandocCitation -> [CitationItem Inlines]
    toCitationItem PandocCitation {..} =
      setCitationMode pandocCitationMode $
        CitationItem
          { citationItemId = ItemId pandocCitationId,
            citationItemLabel = Nothing,
            citationItemLocator = Nothing,
            citationItemType = NormalCite,
            citationItemPrefix = Just (Builder.Many . Seq.fromList $ pandocCitationPrefix),
            citationItemSuffix = Just (Builder.Many . Seq.fromList $ pandocCitationSuffix),
            citationItemData = Nothing
          }
