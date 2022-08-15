{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Development.Shoggoth.Extension where

import Control.Monad (forM, liftM2)
import Control.Monad.Identity (Identity (Identity, runIdentity))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.Default.Class (Default (def))
import Data.Dynamic (Dynamic (Dynamic), fromDyn, fromDynamic, toDyn)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Endo (Endo, appEndo))
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Traversable (for)
import Data.Typeable (TypeRep, Typeable, typeRep)
import Development.Shake (FilePattern, Resource, RuleResult, ShakeOptions (ShakeOptions), ShakeValue)
import Development.Shake qualified as Shake
import Development.Shake.Classes (Hashable)

data SomeValue = forall a. (Typeable a) => SomeValue a

indexValue :: SomeValue -> (TypeRep, Dynamic)
indexValue (SomeValue (value :: a)) = (typeRep (Proxy :: Proxy a), toDyn value)

type ShakeCache q a = (Typeable q, Typeable a, Eq q, Hashable q)

data SomeCache = forall q a. ShakeCache q a => SomeCache (q -> Shake.Action a)

setupCache :: SomeCache -> Shake.Rules (TypeRep, Dynamic)
setupCache (SomeCache cache) = indexValue . SomeValue <$> Shake.newCache cache

type ShakeOracle q a = (Typeable q, Typeable a, RuleResult q ~ a, ShakeValue q, ShakeValue a)

data SomeOracle = forall q a. ShakeOracle q a => SomeOracle (q -> Shake.Action a)

setupOracle :: SomeOracle -> Shake.Rules (TypeRep, Dynamic)
setupOracle (SomeOracle oracle) = indexValue . SomeValue <$> Shake.addOracle oracle

data SomeResource = SomeResource
  { resourceName :: String,
    resourceQuantity :: Int,
    resourceOptionalCooldown :: Maybe Double
  }

setupResource :: SomeResource -> Shake.Rules (String, Shake.Resource)
setupResource SomeResource {..} = do
  resource <- case resourceOptionalCooldown of
    Just resourceCooldown -> Shake.newThrottle resourceName resourceQuantity resourceCooldown
    Nothing -> Shake.newResource resourceName resourceQuantity
  return (resourceName, resource)

data Extension = Extension
  { extensionValues :: [SomeValue],
    extensionOracles :: [SomeOracle],
    extensionResources :: [SomeResource],
    extensionCaches :: [SomeCache]
  }

data ExtensionT (m :: Type -> Type) = ExtensionT
  { shakeValues :: HashMap TypeRep Dynamic,
    shakeOracles :: m (HashMap TypeRep Dynamic),
    shakeResources :: m (Map String Resource),
    shakeCaches :: m (HashMap TypeRep Dynamic)
  }

mapExtensionT :: (forall a. m a -> n a) -> ExtensionT m -> ExtensionT n
mapExtensionT m2n (ExtensionT e o r c) = ExtensionT e (m2n o) (m2n r) (m2n c)

sequenceExtensionT :: Monad m => ExtensionT m -> m ExtensionData
sequenceExtensionT (ExtensionT e o r c) =
  o >>= \o' ->
    r >>= \r' ->
      c >>= \c' ->
        return $ ExtensionT e (Identity o') (Identity r') (Identity c')

instance Monad m => Semigroup (ExtensionT m) where
  ExtensionT e1 o1 r1 c1 <> ExtensionT e2 o2 r2 c2 =
    ExtensionT (e1 <> e2) (liftM2 (<>) o1 o2) (liftM2 (<>) r1 r2) (liftM2 (<>) c1 c2)

instance Monad m => Monoid (ExtensionT m) where
  mempty = ExtensionT mempty (return mempty) (return mempty) (return mempty)

setupExtension :: Extension -> ExtensionT Shake.Rules
setupExtension Extension {..} = ExtensionT {..}
  where
    shakeValues = HashMap.fromList (indexValue <$> extensionValues)
    shakeOracles = HashMap.fromList <$> mapM setupOracle extensionOracles
    shakeResources = Map.fromList <$> mapM setupResource extensionResources
    shakeCaches = HashMap.fromList <$> mapM setupCache extensionCaches

type ExtensionData = ExtensionT Identity

newtype Action a = Action
  { unAction :: ReaderT ExtensionData Shake.Action a
  }

lookupValue :: forall a. Typeable a => Action (Maybe a)
lookupValue = Action $ do
  extensionData@ExtensionT {..} <- ask
  let valueTypeRep = typeRep (Proxy :: Proxy a)
  case HashMap.lookup valueTypeRep shakeValues of
    Nothing -> return Nothing
    Just dy -> return $ fromDynamic dy

liftAction :: Shake.Action a -> Action a
liftAction = Action . lift

lookupCache :: forall q a. ShakeCache q a => Action (Maybe (q -> Action a))
lookupCache = Action $ do
  extensionData@ExtensionT {..} <- ask
  let cacheTypeRep = typeRep (Proxy :: Proxy (q -> Shake.Action a))
  case HashMap.lookup cacheTypeRep (runIdentity shakeCaches) of
    Nothing -> return Nothing
    Just dy -> return $ (liftAction .) <$> fromDynamic dy

lookupOracle :: forall q a. ShakeOracle q a => Action (Maybe (q -> Action a))
lookupOracle = Action $ do
  extensionData@ExtensionT {..} <- ask
  let oracleTypeRep = typeRep (Proxy :: Proxy (q -> Shake.Action a))
  case HashMap.lookup oracleTypeRep (runIdentity shakeOracles) of
    Nothing -> return Nothing
    Just dy -> return $ (liftAction .) <$> fromDynamic dy

data Rules a = Rules
  { shakeRules :: ExtensionData -> Shake.Rules a,
    shakeExtensions :: [Extension]
  }

liftRules :: ((Action r -> Shake.Action r) -> Shake.Rules a) -> Rules a
liftRules shakeRules =
  Rules
    { shakeRules = shakeRules . runAction,
      shakeExtensions = mempty
    }
  where
    runAction :: ExtensionData -> Action r -> Shake.Action r
    runAction extensionData action = runReaderT (unAction action) extensionData

shakeArgs :: ShakeOptions -> Rules () -> IO ()
shakeArgs shakeOptions Rules {..} = do
  let extensions@ExtensionT {..} = foldMap setupExtension shakeExtensions
  Shake.shakeArgs shakeOptions (shakeRules =<< sequenceExtensionT extensions)

(%>) :: FilePattern -> (FilePath -> Action ()) -> Rules ()
filePattern %> action = liftRules $ \runAction -> filePattern Shake.%> (runAction . action)