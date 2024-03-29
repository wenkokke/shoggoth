module Shoggoth.Template.Pandoc
  ( runPandoc,
    runPandocWith,
    Template,
    markdownToPandoc,
    pandocToHtml5,
    processCitations,
    compileTemplate,
    compileTemplateFile,
    renderTemplate,
    applyAsTemplate,
    applyTemplate,
    applyTemplates,
    makeCachedTemplateFileGetter,
    module Pandoc,
    module PandocBuilderTypes,
    module PandocCiteprocTypes,
    HighlightStyle,
    module PandocHighlighting,
    module PandocOptions,
    module PandocHelper,
    module PandocSelfContained,
    shiftHeadersBy,
  )
where

-- Citation (..),
-- CitationMode (..),

import Control.Exception (Exception (displayException))
-- Redefined in Shoggoth.Template.Pandoc.Citeproc

-- Redefined in Shoggoth.Template.Pandoc

import Control.Monad (foldM)
import Data.Aeson (Value (Object))
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Shoggoth.Configuration
import Shoggoth.Metadata (Metadata (..), constField)
import Shoggoth.Prelude (Action, Rules, getShakeExtra, liftEither, liftIO, need, newCache, readFile', (</>))
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
import Shoggoth.Template.Pandoc.Citeproc qualified as PandocCiteproc
import Shoggoth.Template.Pandoc.Helper as PandocHelper
import Text.DocLayout qualified as Doc (render)
import Text.Pandoc as Pandoc hiding
  ( Citation (..),
    CitationMode (..),
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
import Text.Pandoc.Options as PandocOptions
  ( CiteMethod (..),
    EPUBVersion (..),
    HTMLMathMethod (..),
    HTMLSlideVariant (..),
    ObfuscationMethod (..),
    ReaderOptions (..),
    ReferenceLocation (..),
    TopLevelDivision (..),
    TrackChanges (..),
    WrapOption (..),
    WriterOptions (..),
  )
import Text.Pandoc.Readers as Pandoc
import Text.Pandoc.SelfContained as PandocSelfContained
  ( makeDataURI,
    makeSelfContained,
  )
import Text.DocTemplates qualified as Template
import Text.Pandoc.Walk as Pandoc
import Text.Pandoc.Writers as Pandoc
import Text.Printf (printf)

type HighlightStyle = PandocRenamed.Style

runPandoc :: PandocIO a -> Action a
runPandoc = runPandocWith ERROR

runPandocWith :: Verbosity -> PandocIO a -> Action a
runPandocWith v act = do
  resultOrError <- liftIO . runIO $ do
    modifyCommonState (\st -> st {stVerbosity = v})
    act
  liftEither displayException resultOrError

-- * Markdown

markdownToPandoc :: Text -> Action Pandoc
markdownToPandoc text = do
  readerOpts <- getReaderOptions
  runPandoc $ Pandoc.readMarkdown readerOpts text

-- * Html5

pandocToHtml5 :: Pandoc -> Action Text
pandocToHtml5 doc = do
  writerOpts <- getWriterOptions
  runPandoc $ Pandoc.writeHtml5String writerOpts doc

-- * Citations

processCitations :: Pandoc -> Action Pandoc
processCitations doc =
  runPandoc $ PandocCiteproc.processCitations doc

-- * Templates

instance Template.TemplateMonad Action where
  getPartial = readFile'

type Template = Template.Template Text

compileTemplate :: FilePath -> Text -> Action Template
compileTemplate filepath contents = do
  tplOrError <- Template.compileTemplate filepath contents
  liftEither id tplOrError

compileTemplateFile :: FilePath -> Action Template
compileTemplateFile filepath = do
  contents <- readFile' filepath
  compileTemplate filepath contents

renderTemplate :: Template -> Metadata -> Text
renderTemplate template (Metadata obj) =
  Doc.render Nothing (Template.renderTemplate template (Object obj))

applyAsTemplate :: Metadata -> Text -> Action Text
applyAsTemplate metadata template = do
  tpl <- compileTemplate "" template
  return $ renderTemplate tpl metadata

applyTemplate ::
  ( ?getTemplateFile :: FilePath -> Action Template
  ) =>
  FilePath ->
  Metadata ->
  Text ->
  Action Text
applyTemplate templateFile metadata body = do
  template <- ?getTemplateFile templateFile
  return $ renderTemplate template (constField "body" body <> metadata)

applyTemplates ::
  ( ?getTemplateFile :: FilePath -> Action Template
  ) =>
  [FilePath] ->
  Metadata ->
  Text ->
  Action Text
applyTemplates templateFiles metadata body =
  foldM (\body templateFile -> applyTemplate templateFile metadata body) body templateFiles

makeCachedTemplateFileGetter ::
  Rules (FilePath -> Action Template)
makeCachedTemplateFileGetter = newCache $ \inputFile -> do
  templateDir <- getTemplateDirectory
  let inputPath = templateDir </> inputFile
  need [inputPath]
  compileTemplateFile inputPath

-- * Reader & Writer Options

getReaderOptions :: Action ReaderOptions
getReaderOptions = do
  maybeReaderOptions <- getShakeExtra @ReaderOptions
  return . fromMaybe def $ maybeReaderOptions

getWriterOptions :: Action WriterOptions
getWriterOptions = do
  maybeWriterOptions <- getShakeExtra @WriterOptions
  return . fromMaybe def $ maybeWriterOptions

-- * Utilities

shiftHeadersBy :: Walkable Block doc => Int -> doc -> doc
shiftHeadersBy n = walk go
  where
    go (Header l attr body) = Header (l + n) attr body
    go x = x
