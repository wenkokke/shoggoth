module Shoggoth.Routing
  ( Source,
    Output,
    Anchor,
    RoutingTable,
    Stages (..),
    Router (..),
    route,
    route',
    routeSource,
    routeSource',
    routeNext,
    routeNext',
    routePrev,
    routePrev',
    routeAnchor,
    routeAnchor',
    sources,
    sources',
    outputs,
    outputs',
  )
where

-- No pure route from posts/2016-03-01-insertion-sort-in-agda.lagda.md
-- CallStack (from HasCallStack):
--   error, called at shoggoth/agda/Shoggoth/Agda.hs:121:19 in wenkokke-0.1.0.0-inplace-shoggoth-agda:Shoggoth.Agda

import Control.Monad (forM, join, (>=>))
import Control.Monad.Identity (Identity (Identity, runIdentity))
import Data.Bimap qualified as Bimap
import Data.Function (fix)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Set qualified as Set
import Data.String (IsString, fromString)
import Data.Text (Text)
import Shoggoth.Prelude (Action, FilePattern, getDirectoryFiles)
import Text.Printf (printf)
import Control.Monad.Except (MonadError (throwError))

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
          routingTableLinks = links1 <> links2,
          routingTableAnchors = anchors1 <> anchors2,
          routingTableVolatile = case (maybeVolatile1, maybeVolatile2) of
            (Nothing, Nothing) -> Nothing
            (Just volatile1, Nothing) -> Just volatile1
            (Nothing, Just volatile2) -> Just volatile2
            (Just volatile1, Just volatile2) -> Just $ (<>) <$> volatile1 <*> volatile2
        }

instance Monoid RoutingTable where
  mempty = RoutingTable mempty mempty mempty mempty mempty

infixr 5 :>:

infixr 5 :@:

data Stages
  = Output FilePath
  | FilePath :>: Stages
  | Anchor :@: Stages

instance IsString Stages where
  fromString = Output

composeStages :: Stages -> [([Anchor], FilePath)]
composeStages = composeStagesAcc []
  where
    composeStagesAcc :: [Anchor] -> Stages -> [([Anchor], FilePath)]
    composeStagesAcc anchors (Output output) = [(anchors, output)]
    composeStagesAcc anchors (stage :>: stages) = (anchors, stage) : composeStages stages
    composeStagesAcc anchors (anchor :@: stages) = composeStagesAcc (anchor : anchors) stages

infix 3 |->

infix 3 %->

class Router router where
  (|->) :: [Source] -> router -> RoutingTable
  (%->) :: [FilePattern] -> router -> RoutingTable
  sourcePatterns %-> router =
    mempty
      { routingTableVolatile = Just $ do
          sources <- getDirectoryFiles "" sourcePatterns
          return $ sources |-> router
      }

instance Router Output where
  [source] |-> output =
    mempty
      { routingTableSources = Set.singleton source,
        routingTableOutputs = Set.singleton output,
        routingTableLinks = Bimap.singleton source output
      }
  _ |-> output =
    error "Cannot route multiple sources to a single output"

instance Router (Source -> Stages) where
  sources |-> stagesFor =
    mconcat $ do
      source <- sources
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

instance Router (Source -> Either String Stages) where
  sources |-> router =
    either error id $ do
      routingTables <- traverse (\source -> do stages <- router source; return ([source] |-> const @Stages @Source stages)) sources
      return . mconcat $ routingTables

instance Router (Source -> Action Stages) where
  sources |-> router =
    mempty
      { routingTableVolatile = Just $ do
          routingTables <- traverse (\source -> do stages <- router source; return ([source] |-> const @Stages @Source stages)) sources
          return . mconcat $ routingTables
      }

-- * Forward routing

route :: (?routingTable :: RoutingTable) => FilePath -> Action Output
route current = iterM step =<< routeNext current
  where
    step current = routeNextPureFirst current ?routingTable

route' :: (MonadError String m, ?routingTable :: RoutingTable) => FilePath -> m Output
route' current = runIdentity . iterM step <$> routeNext' current
  where
    step current = Identity $ routeNextPureOnly current ?routingTable

routeNext :: (?routingTable :: RoutingTable) => FilePath -> Action FilePath
routeNext current =
  either (\_ -> fail $ printf "No route from %s" current) return
    =<< routeNextPureFirst current ?routingTable

routeNext' :: (MonadError String m, ?routingTable :: RoutingTable) => FilePath -> m FilePath
routeNext' current = routeNextPureOnly current ?routingTable

routeNextPureFirst :: FilePath -> RoutingTable -> Action (Either String FilePath)
routeNextPureFirst current = shortCircuit (routeNextPureOnly current)

routeNextPureOnly :: MonadError String m => FilePath -> RoutingTable -> m FilePath
routeNextPureOnly current routingTable =
  maybe (throwError $ printf "No pure route from %s" current) return $
    Bimap.lookup current (routingTableLinks routingTable)

-- * Backward routing

routeSource :: (?routingTable :: RoutingTable) => FilePath -> Action Source
routeSource current = iterM step =<< routePrev current
  where
    step current = routePrevPureFirst current ?routingTable

routeSource' :: (MonadError String m, ?routingTable :: RoutingTable) => FilePath -> m Source
routeSource' current = runIdentity . iterM step <$> routePrev' current
  where
    step current = Identity $ routePrevPureOnly current ?routingTable

routePrev :: (?routingTable :: RoutingTable) => FilePath -> Action FilePath
routePrev current =
  either (\_ -> fail $ printf "No route to %s" current) return
    =<< routePrevPureFirst current ?routingTable

routePrev' :: (MonadError String m, ?routingTable :: RoutingTable) => FilePath -> m FilePath
routePrev' current =
  routePrevPureOnly current ?routingTable

routePrevPureFirst :: FilePath -> RoutingTable -> Action (Either String FilePath)
routePrevPureFirst current = shortCircuit (routePrevPureOnly current)

routePrevPureOnly :: MonadError String m => FilePath -> RoutingTable -> m FilePath
routePrevPureOnly current routingTable =
  maybe (throwError $ printf "No pure route to %s" current) return $
    Bimap.lookupR current (routingTableLinks routingTable)

-- * Anchors

routeAnchor :: (?routingTable :: RoutingTable) => Anchor -> Source -> Action FilePath
routeAnchor anchor source = routeAnchorPureFirst anchor source ?routingTable

routeAnchor' :: (MonadError String m, ?routingTable :: RoutingTable) => Anchor -> Source -> m FilePath
routeAnchor' anchor source = routeAnchorPureOnly anchor source ?routingTable

routeAnchorPureFirst :: Anchor -> Source -> RoutingTable -> Action FilePath
routeAnchorPureFirst anchor source routingTable =
  either (\_ -> fail $ printf "No anchor %s for %s" (show anchor) source) return
    =<< shortCircuit (routeAnchorPureOnly anchor source) routingTable

routeAnchorPureOnly :: MonadError String m => Anchor -> Source -> RoutingTable -> m FilePath
routeAnchorPureOnly anchor source routingTable =
  maybe (throwError $ printf "No pure anchor %s for %s" (show anchor) source) return $
    Map.lookup (anchor, source) (routingTableAnchors routingTable)

outputs :: (?routingTable :: RoutingTable) => Action [Output]
outputs = Set.toAscList <$> gather routingTableOutputs ?routingTable

outputs' :: (?routingTable :: RoutingTable) => [Output]
outputs' = Set.toAscList $ routingTableOutputs ?routingTable

sources :: (?routingTable :: RoutingTable) => Action [Source]
sources = Set.toAscList <$> gather routingTableSources ?routingTable

sources' :: (?routingTable :: RoutingTable) => [Source]
sources' = Set.toAscList $ routingTableSources ?routingTable

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

instance (Ord a, Ord b) => Semigroup (Bimap.Bimap a b) where
  bm1 <> bm2 = foldr (uncurry Bimap.insert) bm1 (Bimap.assocs bm2)

instance (Ord a, Ord b) => Monoid (Bimap.Bimap a b) where
  mempty = Bimap.empty
