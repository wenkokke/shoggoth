module Shoggoth.Routing
  ( Source,
    Output,
    Anchor,
    RoutingTable,
    create,
    Stages (..),
    pattern (:?),
    Router (..),
    route,
    output,
    routeUrl,
    url,
    routeSource,
    source,
    routeNext,
    next,
    routePrev,
    prev,
    routeAnchor,
    anchor,
    sources,
    outputs,
    permalinkRouter,
  )
where

import Control.Arrow (Arrow (second))
import Control.Monad (forM, join, (>=>))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Identity (Identity (Identity, runIdentity))
import Data.Bimap qualified as Bimap
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Set qualified as Set
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import Shoggoth.Configuration (getOutputDirectory)
import Shoggoth.Metadata (readYamlFrontmatter, (^.))
import Shoggoth.Prelude (Action, FilePattern, Url, getDirectoryFiles, getShakeExtra, makeRelative, normaliseEx)
import Shoggoth.Prelude.FilePath ((</>))
import Text.Printf (printf)

-- | Route files based on their permalink.
permalinkRouter :: FilePath -> Action FilePath
permalinkRouter src = do
  outDir <- getOutputDirectory
  yamlFrontmatter <- readYamlFrontmatter src
  permalink <- either fail return $ yamlFrontmatter ^. "permalink"
  if permalink == "/"
    then return $ outDir </> "index.html"
    else do
      let out = outDir </> removeLeadingSlash (Text.unpack permalink)
      let outIsDir = "/" `List.isSuffixOf` out
      return $ if outIsDir then out </> "index.html" else out

removeLeadingSlash :: FilePath -> FilePath
removeLeadingSlash src
  | "/" `List.isPrefixOf` src = tail src
  | otherwise = src

getRoutingTable :: Action RoutingTable
getRoutingTable = do
  maybeRoutingTable <- getShakeExtra @RoutingTable
  case maybeRoutingTable of
    Just routingTable -> return routingTable
    Nothing -> fail "Error: Missing `RoutingTable` from `shakeExtra`"

type Anchor = Text

type Source = FilePath

type Output = FilePath

data RoutingTable = RoutingTable
  { routingTableSources :: Set.Set Source,
    routingTableOutputs :: Set.Set Output,
    routingTableLinks :: Bimap.Bimap FilePath FilePath,
    routingTableAnchors :: Map.Map (Anchor, Source) FilePath,
    routingTableVolatile :: Maybe (Action RoutingTable)
  }

instance Semigroup RoutingTable where
  RoutingTable sources1 outputs1 links1 anchors1 maybeVolatile1
    <> RoutingTable sources2 outputs2 links2 anchors2 maybeVolatile2 =
      RoutingTable
        { routingTableSources = sources1 <> sources2,
          routingTableOutputs = outputs1 <> outputs2,
          routingTableLinks = links1 `bimapMerge` links2,
          routingTableAnchors = anchors1 <> anchors2,
          routingTableVolatile = case (maybeVolatile1, maybeVolatile2) of
            (Nothing, Nothing) -> Nothing
            (Just volatile1, Nothing) -> Just volatile1
            (Nothing, Just volatile2) -> Just volatile2
            (Just volatile1, Just volatile2) -> Just $ (<>) <$> volatile1 <*> volatile2
        }

instance Monoid RoutingTable where
  mempty = RoutingTable mempty mempty memptyBimap mempty mempty

bimapMerge :: (Ord a, Ord b) => Bimap.Bimap a b -> Bimap.Bimap a b -> Bimap.Bimap a b
bimapMerge bm1 bm2 = foldr (uncurry Bimap.insert) bm1 (Bimap.assocs bm2)

memptyBimap :: Bimap.Bimap a b
memptyBimap = Bimap.empty

infixr 5 :>

infixr 5 :@

data Stages
  = Output FilePath
  | FilePath :> Stages
  | Anchor :@ Stages

pattern (:?) :: Stages -> Anchor -> Stages
pattern stages :? anchor = anchor :@ stages

instance IsString Stages where
  fromString = Output

composeStages :: Stages -> [([Anchor], FilePath)]
composeStages stages = second normaliseEx <$> composeStagesAcc [] stages
  where
    composeStagesAcc :: [Anchor] -> Stages -> [([Anchor], FilePath)]
    composeStagesAcc anchors (Output output) = [(anchors, output)]
    composeStagesAcc anchors (stage :> stages) = (anchors, stage) : composeStages stages
    composeStagesAcc anchors (anchor :@ stages) = composeStagesAcc (anchor : anchors) stages

create :: FilePath -> RoutingTable
create out = mempty {routingTableOutputs = Set.singleton out}

infix 3 |->

infix 3 *|->

class Router router where
  (|->) :: [Source] -> router -> RoutingTable
  (*|->) :: [FilePattern] -> router -> RoutingTable
  sourcePatterns *|-> router =
    mempty
      { routingTableVolatile = Just $ do
          sources <- getDirectoryFiles "" sourcePatterns
          return $ sources |-> router
      }

instance Router Output where
  [source] |-> output =
    let normalSource = normaliseEx source
     in let normalOutput = normaliseEx output
         in mempty
              { routingTableSources = Set.singleton normalSource,
                routingTableOutputs = Set.singleton normalOutput,
                routingTableLinks = Bimap.singleton normalSource normalOutput
              }
  _ |-> output =
    error $ "Cannot route multiple sources to single output " <> output

instance Router (Source -> Stages) where
  sources |-> stagesFor =
    mconcat $ do
      source <- normaliseEx <$> sources
      let stages = composeStages (stagesFor source)
      let output = snd (last stages)
      let links = zip (source : map snd stages) (map snd stages)
      let anchors = [((anchor, source), stage) | (anchors, stage) <- stages, anchor <- anchors]
      return
        mempty
          { routingTableSources = Set.singleton source,
            routingTableOutputs = Set.singleton output,
            routingTableLinks = Bimap.fromList links,
            routingTableAnchors = Map.fromList anchors
          }

instance Router (Source -> Output) where
  sources |-> outputFor =
    sources |-> \source -> Output (outputFor source)

instance Router (Source -> Either String Stages) where
  sources |-> router =
    either error id $ do
      routingTables <- for sources $ \source -> do
        stages <- router source
        return $ [source] |-> const @Stages @Source stages
      return . mconcat $ routingTables

instance Router (Source -> Action Stages) where
  sources |-> router =
    mempty
      { routingTableVolatile = Just $ do
          routingTables <- for sources $ \source -> do
            stages <- router source
            return $ [source] |-> const @Stages @Source stages
          return . mconcat $ routingTables
      }

instance Router (Source -> Action Output) where
  sources |-> router =
    sources |-> (fmap Output . router)

-- * Forward routing

{-# DEPRECATED route "Use output instead." #-}
route :: (?routingTable :: RoutingTable) => FilePath -> Action Output
route = output

output :: (?routingTable :: RoutingTable) => FilePath -> Action Output
output current = do
  iterM step =<< routeNext current
  where
    step current = routeNextPureFirst current ?routingTable

{-# DEPRECATED routeUrl "Use url instead." #-}
routeUrl :: (?routingTable :: RoutingTable) => FilePath -> Action Url
routeUrl = url

url :: (?routingTable :: RoutingTable) => FilePath -> Action Url
url current = do
  outDir <- getOutputDirectory
  out <- iterM (`routeNextPureFirst` ?routingTable) current
  return . Text.pack $ "/" <> makeRelative outDir out

{-# DEPRECATED routeNext "Use next instead." #-}
routeNext :: (?routingTable :: RoutingTable) => FilePath -> Action FilePath
routeNext = next

next :: (?routingTable :: RoutingTable) => FilePath -> Action FilePath
next current =
  either (\_ -> fail $ printf "No route from %s" current) return
    =<< routeNextPureFirst current ?routingTable

routeNextPureFirst :: FilePath -> RoutingTable -> Action (Either String FilePath)
routeNextPureFirst current = shortCircuit (routeNextPureOnly current)

routeNextPureOnly :: MonadError String m => FilePath -> RoutingTable -> m FilePath
routeNextPureOnly current routingTable =
  let normalCurrent = normaliseEx current
   in maybe (throwError $ printf "No pure route from %s" normalCurrent) return $
        Bimap.lookup normalCurrent (routingTableLinks routingTable)

-- * Backward routing

{-# DEPRECATED routeSource "Use source instead." #-}
routeSource :: (?routingTable :: RoutingTable) => FilePath -> Action Source
routeSource = source

source :: (?routingTable :: RoutingTable) => FilePath -> Action Source
source current = iterM step current
  where
    step current = routePrevPureFirst current ?routingTable

{-# DEPRECATED routePrev "Use prev instead." #-}
routePrev :: (?routingTable :: RoutingTable) => FilePath -> Action FilePath
routePrev = prev

prev :: (?routingTable :: RoutingTable) => FilePath -> Action FilePath
prev current =
  either (\_ -> fail $ printf "No route to %s" current) return
    =<< routePrevPureFirst current ?routingTable

routePrevPureFirst :: FilePath -> RoutingTable -> Action (Either String FilePath)
routePrevPureFirst current = shortCircuit (routePrevPureOnly current)

routePrevPureOnly :: MonadError String m => FilePath -> RoutingTable -> m FilePath
routePrevPureOnly current routingTable =
  let normalCurrent = normaliseEx current
   in maybe (throwError $ printf "No pure route to %s" normalCurrent) return $
        Bimap.lookupR normalCurrent (routingTableLinks routingTable)

-- * Anchors

{-# DEPRECATED routeAnchor "Use anchor instead." #-}
routeAnchor :: (?routingTable :: RoutingTable) => Anchor -> Source -> Action FilePath
routeAnchor = anchor

anchor :: (?routingTable :: RoutingTable) => Anchor -> Source -> Action FilePath
anchor anchor source = routeAnchorPureFirst anchor source ?routingTable

routeAnchorPureFirst :: Anchor -> Source -> RoutingTable -> Action FilePath
routeAnchorPureFirst anchor source routingTable =
  either (\_ -> fail $ printf "No anchor %s for %s" (show anchor) source) return
    =<< shortCircuit (routeAnchorPureOnly anchor source) routingTable

routeAnchorPureOnly :: MonadError String m => Anchor -> Source -> RoutingTable -> m FilePath
routeAnchorPureOnly anchor source routingTable =
  let normalSource = normaliseEx source
   in maybe (throwError $ printf "No pure anchor %s for %s" (show anchor) normalSource) return $
        Map.lookup (anchor, normalSource) (routingTableAnchors routingTable)

-- * All source or output files

outputs :: (?routingTable :: RoutingTable) => Action [Output]
outputs = Set.toAscList <$> gather routingTableOutputs ?routingTable

sources :: (?routingTable :: RoutingTable) => Action [Source]
sources = Set.toAscList <$> gather routingTableSources ?routingTable

-- * Helpers

shortCircuit :: (RoutingTable -> Either String result) -> RoutingTable -> Action (Either String result)
shortCircuit op routingTable = do
  case op routingTable of
    resultPure@(Right _) -> return resultPure
    Left errorMessage ->
      case routingTableVolatile routingTable of
        Nothing -> return $ Left "No volatile routes"
        Just getRoutingTable -> do
          routingTableVolatile <- getRoutingTable
          shortCircuit op routingTableVolatile

gather :: Monoid result => (RoutingTable -> result) -> RoutingTable -> Action result
gather op routingTable = do
  let resultPure = op routingTable
  case routingTableVolatile routingTable of
    Nothing -> return resultPure
    Just getRoutingTable -> do
      routingTableVolatile <- getRoutingTable
      resultVolatile <- gather op routingTableVolatile
      return $ resultPure <> resultVolatile

iterM :: Monad m => (result -> m (Either error result)) -> result -> m result
iterM stepM current =
  either (\_ -> return current) (iterM stepM) =<< stepM current
