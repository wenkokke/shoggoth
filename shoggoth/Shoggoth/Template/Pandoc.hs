module Shoggoth.Template.Pandoc
  ( runPandoc,
    module Pandoc,
    module PandocBuilderTypes,
    module PandocCiteprocTypes,
    HighlightStyle,
    module PandocHighlighting,
    module PandocPostprocess,
    module PandocUrl,
    shiftHeadersBy,
  )
where

-- Citation (..),
-- CitationMode (..),

import Control.Exception (Exception (displayException))
import Data.Text (Text)
import Shoggoth.Prelude (Action, liftEither, liftIO)
import Shoggoth.Template.Pandoc.Builder as PandocBuilderTypes
  ( Alignment (..),
    Attr,
    Block (..),
    Blocks,
    Caption (..),
    Cell (..),
    ColSpan (..),
    ColSpec,
    ColWidth (..),
    Format (..),
    HasMeta (..),
    Inline (..),
    Inlines,
    ListAttributes,
    ListNumberDelim (..),
    ListNumberStyle (..),
    Many (..),
    MathType (..),
    Meta (..),
    MetaValue (..),
    Pandoc (..),
    QuoteType (..),
    Row (..),
    RowHeadColumns (..),
    RowSpan (..),
    ShortCaption,
    TableBody (..),
    TableFoot (..),
    TableHead (..),
    Target,
    ToMetaValue (..),
  )
import Shoggoth.Template.Pandoc.Citeproc as PandocCiteprocTypes
  ( Abbreviations,
    Citation (..),
    CitationItem (..),
    CitationItemType (..),
    CiteprocError (..),
    CiteprocOptions (..),
    CiteprocOutput (..),
    Collapsing (..),
    Condition (..),
    DP (..),
    DPForm (..),
    DPName (..),
    Date (..),
    DateParts (..),
    DateType (..),
    DelimiterPrecedes (..),
    DemoteNonDroppingParticle (..),
    DisambiguationData (..),
    DisambiguationStrategy (..),
    DisplayStyle (..),
    Element (..),
    ElementType (..),
    FontStyle (..),
    FontVariant (..),
    FontWeight (..),
    Formatting (..),
    GivenNameDisambiguationRule (..),
    Identifier (..),
    Inputs (..),
    ItemId (..),
    Lang (..),
    Layout (..),
    LayoutOptions (..),
    Locale (..),
    Match (..),
    Name (..),
    NameAsSortOrder (..),
    NameForm (..),
    NameFormat (..),
    NameHints (..),
    NamesFormat (..),
    NumberForm (..),
    Output (..),
    PageRangeFormat (..),
    Pluralize (..),
    Position (..),
    Reference (..),
    ReferenceMap (..),
    Result (..),
    SecondFieldAlign (..),
    ShowDateParts (..),
    SortDirection (..),
    SortKey (..),
    SortKeyValue (..),
    Style (..),
    StyleOptions (..),
    SubsequentAuthorSubstitute (..),
    SubsequentAuthorSubstituteRule (..),
    Tag (..),
    Term (..),
    TermForm (..),
    TermGender (..),
    TermMatch (..),
    TermNumber (..),
    TextCase (..),
    TextDecoration (..),
    TextType (..),
    Val (..),
    Variable,
    VariableForm (..),
    VariableType (..),
    VerticalAlign (..),
  )
import Shoggoth.Template.Pandoc.Postprocess as PandocPostprocess
import Shoggoth.Template.Pandoc.Url as PandocUrl
import Text.Pandoc as Pandoc hiding
  ( -- Redefined in Shoggoth.Template.Pandoc.Citeproc
    Citation (..),
    CitationMode (..),
    -- Redefined in Shoggoth.Template.Template
    Template,
    compileTemplate,
    getTemplateFile,
    renderTemplate,
  )
import Text.Pandoc.Class (PandocIO, runIO)
import Text.Pandoc.Highlighting as PandocHighlighting
  ( breezeDark,
    espresso,
    haddock,
    kate,
    monochrome,
    pygments,
    styleToCss,
    tango,
    zenburn,
  )
import Text.Pandoc.Highlighting qualified as PandocRenamed
import Text.Pandoc.Readers as Pandoc
import Text.Pandoc.Walk as Pandoc
import Text.Pandoc.Writers as Pandoc

type HighlightStyle = PandocRenamed.Style

runPandoc :: PandocIO a -> Action a
runPandoc act = do
  resultOrError <- liftIO (runIO act)
  liftEither displayException resultOrError

shiftHeadersBy :: Walkable Block doc => Int -> doc -> doc
shiftHeadersBy n = walk go
  where
    go (Header l attr body) = Header (l + n) attr body
    go x = x