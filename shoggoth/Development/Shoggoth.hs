{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Development.Shoggoth
  ( MonadIO (..),
    Shake.ShakeException (..),
    Shake.ShakeOptions (..),
    Shake.Rebuild (..),
    Shake.Lint (..),
    Shake.Change (..),
    Shake.Progress (..),
    Shake.Verbosity (..),
    Shake.Stdout (..),
    Shake.StdoutTrim (..),
    Shake.Stderr (..),
    Shake.Stdouterr (..),
    Shake.Exit (..),
    Shake.Process (..),
    Shake.CmdTime (..),
    Shake.CmdLine (..),
    Shake.FSATrace (..),
    Shake.CmdResult,
    Shake.CmdString,
    Shake.CmdOption (..),
    Shake.FilePattern,
    Shake.ShakeValue,
    Shake.RuleResult,
    Shake.Resource,
    shake,
    shakeOptions,
    action,
    withoutActions,
    alternatives,
    priority,
    versioned,
    traced,
    actionOnException,
    actionFinally,
    actionBracket,
    actionCatch,
    actionRetry,
    runAfter,
    getShakeOptions,
    getShakeOptionsRules,
    getHashedShakeVersion,
    getShakeExtra,
    getShakeExtraRules,
    addShakeExtra,
    shakeArgs,
    shakeArgsWith,
    shakeArgsOptionsWith,
    shakeOptDescrs,
    addHelpSuffix,
    getTargets,
    addTarget,
    withTargetDocs,
    withoutTargets,
    Shake.progressSimple,
    Shake.progressDisplay,
    Shake.progressTitlebar,
    Shake.progressProgram,
    getProgress,
    getVerbosity,
    putVerbose,
    putInfo,
    putWarn,
    putError,
    withVerbosity,
    quietly,
    command,
    command_,
    unit,
    addPath,
    addEnv,
    parallel,
    forP,
    par,
    copyFile',
    copyFileChanged,
    readFile',
    readFileLines,
    writeFile',
    writeFileLines,
    writeFileChanged,
    removeFiles,
    removeFilesAfter,
    withTempFile,
    withTempDir,
    withTempFileWithin,
    withTempDirWithin,
    need,
    want,
    (%>),
    (|%>),
    (?>),
    phony,
    (~>),
    phonys,
    (&%>),
    (&?>),
    orderOnly,
    orderOnlyAction,
    (Shake.?==),
    (Shake.<//>),
    Shake.filePattern,
    needed,
    trackRead,
    trackWrite,
    trackAllow,
    doesFileExist,
    doesDirectoryExist,
    getDirectoryContents,
    getDirectoryFiles,
    getDirectoryDirs,
    Shake.getDirectoryFilesIO,
    getEnv,
    getEnvWithDefault,
    getEnvError,
    addOracle,
    addOracleCache,
    addOracleHash,
    askOracle,
    askOracles,
    alwaysRerun,
    newResource,
    Shake.newResourceIO,
    withResource,
    withResources,
    newThrottle,
    Shake.newThrottleIO,
    unsafeExtraThread,
    newCache,
    Shake.newCacheIO,
    historyDisable,
    produces,
    needHasChanged,
    resultHasChanged,
    batch,
    reschedule,
    putLoud,
    putNormal,
    putQuiet,
  )
where

import Control.Exception (Exception)
import Control.Exception.Extra (Partial)
import Control.Monad.IO.Class (MonadIO)
import Data.Dynamic (Dynamic)
import Data.HashMap.Strict (HashMap)
import Data.Typeable (TypeRep, Typeable)
import Development.Shake (FilePattern, RuleResult, ShakeValue)
import Development.Shake qualified as Shake
import Development.Shake.Classes (Hashable)
import Development.Shake.Command (CmdArguments, CmdOption, CmdResult, type (:->))
import Development.Shoggoth.Extension (Action, Rules, liftAction, liftRules)
import System.Console.GetOpt (OptDescr)

shake :: Shake.ShakeOptions -> Shake.Rules () -> IO ()
shake = Shake.shake

shakeOptions :: Shake.ShakeOptions
shakeOptions = Shake.shakeOptions

action :: Shake.Action a -> Shake.Rules ()
action = Shake.action

withoutActions :: Shake.Rules a -> Shake.Rules a
withoutActions = Shake.withoutActions

alternatives :: Shake.Rules a -> Shake.Rules a
alternatives = Shake.alternatives

priority :: Double -> Shake.Rules a -> Shake.Rules a
priority = Shake.priority

versioned :: Int -> Shake.Rules a -> Shake.Rules a
versioned = Shake.versioned

traced :: String -> IO a -> Shake.Action a
traced = Shake.traced

actionOnException :: Shake.Action a -> IO b -> Shake.Action a
actionOnException = Shake.actionOnException

actionFinally :: Shake.Action a -> IO b -> Shake.Action a
actionFinally = Shake.actionFinally

actionBracket :: IO a -> (a -> IO b) -> (a -> Shake.Action c) -> Shake.Action c
actionBracket = Shake.actionBracket

actionCatch :: forall e a. Exception e => Shake.Action a -> (e -> Shake.Action a) -> Shake.Action a
actionCatch = Shake.actionCatch

actionRetry :: Int -> Shake.Action a -> Shake.Action a
actionRetry = Shake.actionRetry

runAfter :: IO () -> Shake.Action ()
runAfter = Shake.runAfter

getShakeOptions :: Shake.Action Shake.ShakeOptions
getShakeOptions = Shake.getShakeOptions

getShakeOptionsRules :: Shake.Rules Shake.ShakeOptions
getShakeOptionsRules = Shake.getShakeOptionsRules

getHashedShakeVersion :: [FilePath] -> IO String
getHashedShakeVersion = Shake.getHashedShakeVersion

getShakeExtra :: forall a. Typeable a => Shake.Action (Maybe a)
getShakeExtra = Shake.getShakeExtra

getShakeExtraRules :: forall a. Typeable a => Shake.Rules (Maybe a)
getShakeExtraRules = Shake.getShakeExtraRules

addShakeExtra :: forall a. Typeable a => a -> HashMap TypeRep Dynamic -> HashMap TypeRep Dynamic
addShakeExtra = Shake.addShakeExtra

shakeArgs :: Shake.ShakeOptions -> Shake.Rules () -> IO ()
shakeArgs = Shake.shakeArgs

shakeArgsWith :: Shake.ShakeOptions -> [OptDescr (Either String a)] -> ([a] -> [String] -> IO (Maybe (Shake.Rules ()))) -> IO ()
shakeArgsWith = Shake.shakeArgsWith

shakeArgsOptionsWith :: Shake.ShakeOptions -> [OptDescr (Either String a)] -> (Shake.ShakeOptions -> [a] -> [String] -> IO (Maybe (Shake.ShakeOptions, Shake.Rules ()))) -> IO ()
shakeArgsOptionsWith = Shake.shakeArgsOptionsWith

shakeOptDescrs :: [OptDescr (Either String (Shake.ShakeOptions -> Shake.ShakeOptions))]
shakeOptDescrs = Shake.shakeOptDescrs

addHelpSuffix :: String -> Shake.Rules ()
addHelpSuffix = Shake.addHelpSuffix

getTargets :: Shake.ShakeOptions -> Shake.Rules () -> IO [(String, Maybe String)]
getTargets = Shake.getTargets

addTarget :: String -> Shake.Rules ()
addTarget = Shake.addTarget

withTargetDocs :: String -> Shake.Rules () -> Shake.Rules ()
withTargetDocs = Shake.withTargetDocs

withoutTargets :: Shake.Rules () -> Shake.Rules ()
withoutTargets = Shake.withoutTargets

getProgress :: Shake.Action Shake.Progress
getProgress = Shake.getProgress

getVerbosity :: Shake.Action Shake.Verbosity
getVerbosity = Shake.getVerbosity

putVerbose :: String -> Shake.Action ()
putVerbose = Shake.putVerbose

putInfo :: String -> Shake.Action ()
putInfo = Shake.putInfo

putWarn :: String -> Shake.Action ()
putWarn = Shake.putWarn

putError :: String -> Shake.Action ()
putError = Shake.putError

withVerbosity :: Shake.Verbosity -> Shake.Action a -> Shake.Action a
withVerbosity = Shake.withVerbosity

quietly :: Shake.Action a -> Shake.Action a
quietly = Shake.quietly

command :: forall r. (Partial, CmdResult r) => [CmdOption] -> String -> [String] -> Shake.Action r
command = Shake.command

command_ :: Partial => [CmdOption] -> String -> [String] -> Shake.Action ()
command_ = Shake.command_

unit :: m () -> m ()
unit = Shake.unit

addPath :: forall (m :: * -> *). MonadIO m => [String] -> [String] -> m CmdOption
addPath = Shake.addPath

addEnv :: forall (m :: * -> *). MonadIO m => [(String, String)] -> m CmdOption
addEnv = Shake.addEnv

parallel :: [Shake.Action a] -> Shake.Action [a]
parallel = Shake.parallel

forP :: [a] -> (a -> Shake.Action b) -> Shake.Action [b]
forP = Shake.forP

par :: Shake.Action a -> Shake.Action b -> Shake.Action (a, b)
par = Shake.par

copyFile' :: Partial => FilePath -> FilePath -> Shake.Action ()
copyFile' = Shake.copyFile'

copyFileChanged :: Partial => FilePath -> FilePath -> Shake.Action ()
copyFileChanged = Shake.copyFileChanged

readFile' :: Partial => FilePath -> Shake.Action String
readFile' = Shake.readFile'

readFileLines :: Partial => FilePath -> Shake.Action [String]
readFileLines = Shake.readFileLines

writeFile' :: forall (m :: * -> *). (Partial, MonadIO m) => FilePath -> String -> m ()
writeFile' = Shake.writeFile'

writeFileLines :: forall (m :: * -> *). (Partial, MonadIO m) => FilePath -> [String] -> m ()
writeFileLines = Shake.writeFileLines

writeFileChanged :: forall (m :: * -> *). (Partial, MonadIO m) => FilePath -> String -> m ()
writeFileChanged = Shake.writeFileChanged

removeFiles :: FilePath -> [FilePattern] -> IO ()
removeFiles = Shake.removeFiles

removeFilesAfter :: FilePath -> [FilePattern] -> Shake.Action ()
removeFilesAfter = Shake.removeFilesAfter

withTempFile :: (FilePath -> Shake.Action a) -> Shake.Action a
withTempFile = Shake.withTempFile

withTempDir :: (FilePath -> Shake.Action a) -> Shake.Action a
withTempDir = Shake.withTempDir

withTempFileWithin :: FilePath -> (FilePath -> Shake.Action a) -> Shake.Action a
withTempFileWithin = Shake.withTempFileWithin

withTempDirWithin :: FilePath -> (FilePath -> Shake.Action a) -> Shake.Action a
withTempDirWithin = Shake.withTempDirWithin

need :: [FilePath] -> Shake.Action ()
need = Shake.need

want :: [FilePath] -> Shake.Rules ()
want = Shake.want

(%>) :: FilePattern -> (FilePath -> Shake.Action ()) -> Shake.Rules ()
(%>) = (Shake.%>)

(|%>) :: [FilePattern] -> (FilePath -> Shake.Action ()) -> Shake.Rules ()
(|%>) = (Shake.|%>)

(?>) :: (FilePath -> Bool) -> (FilePath -> Shake.Action ()) -> Shake.Rules ()
(?>) = (Shake.?>)

phony :: String -> Shake.Action () -> Shake.Rules ()
phony = Shake.phony

(~>) :: String -> Shake.Action () -> Shake.Rules ()
(~>) = (Shake.~>)

phonys :: (String -> Maybe (Shake.Action ())) -> Shake.Rules ()
phonys = Shake.phonys

(&%>) :: [FilePattern] -> ([FilePath] -> Shake.Action ()) -> Shake.Rules ()
(&%>) = (Shake.&%>)

(&?>) :: (FilePath -> Maybe [FilePath]) -> ([FilePath] -> Shake.Action ()) -> Shake.Rules ()
(&?>) = (Shake.&?>)

orderOnly :: [FilePath] -> Shake.Action ()
orderOnly = Shake.orderOnly

orderOnlyAction :: Shake.Action a -> Shake.Action a
orderOnlyAction = Shake.orderOnlyAction

needed :: [FilePath] -> Shake.Action ()
needed = Shake.needed

trackRead :: [FilePath] -> Shake.Action ()
trackRead = Shake.trackRead

trackWrite :: [FilePath] -> Shake.Action ()
trackWrite = Shake.trackWrite

trackAllow :: [FilePattern] -> Shake.Action ()
trackAllow = Shake.trackAllow

doesFileExist :: FilePath -> Shake.Action Bool
doesFileExist = Shake.doesFileExist

doesDirectoryExist :: FilePath -> Shake.Action Bool
doesDirectoryExist = Shake.doesDirectoryExist

getDirectoryContents :: FilePath -> Shake.Action [FilePath]
getDirectoryContents = Shake.getDirectoryContents

getDirectoryFiles :: FilePath -> [FilePattern] -> Shake.Action [FilePath]
getDirectoryFiles = Shake.getDirectoryFiles

getDirectoryDirs :: FilePath -> Shake.Action [FilePath]
getDirectoryDirs = Shake.getDirectoryDirs

getEnv :: String -> Shake.Action (Maybe String)
getEnv = Shake.getEnv

getEnvWithDefault :: String -> String -> Shake.Action String
getEnvWithDefault = Shake.getEnvWithDefault

getEnvError :: String -> Shake.Action String
getEnvError = Shake.getEnvError

addOracle :: forall q a. (Partial, RuleResult q ~ a, ShakeValue q, ShakeValue a) => (q -> Shake.Action a) -> Shake.Rules (q -> Shake.Action a)
addOracle = Shake.addOracle

addOracleCache :: forall q a. (RuleResult q ~ a, ShakeValue q, ShakeValue a, Partial) => (q -> Shake.Action a) -> Shake.Rules (q -> Shake.Action a)
addOracleCache = Shake.addOracleCache

addOracleHash :: forall q a. (RuleResult q ~ a, ShakeValue q, ShakeValue a, Partial) => (q -> Shake.Action a) -> Shake.Rules (q -> Shake.Action a)
addOracleHash = Shake.addOracleHash

askOracle :: forall q a. (RuleResult q ~ a, ShakeValue q, ShakeValue a) => q -> Shake.Action a
askOracle = Shake.askOracle

askOracles :: forall q a. (RuleResult q ~ a, ShakeValue q, ShakeValue a) => [q] -> Shake.Action [a]
askOracles = Shake.askOracles

alwaysRerun :: Shake.Action ()
alwaysRerun = Shake.alwaysRerun

newResource :: String -> Int -> Shake.Rules Shake.Resource
newResource = Shake.newResource

withResource :: Shake.Resource -> Int -> Shake.Action a -> Shake.Action a
withResource = Shake.withResource

withResources :: [(Shake.Resource, Int)] -> Shake.Action a -> Shake.Action a
withResources = Shake.withResources

newThrottle :: String -> Int -> Double -> Shake.Rules Shake.Resource
newThrottle = Shake.newThrottle

unsafeExtraThread :: Shake.Action a -> Shake.Action a
unsafeExtraThread = Shake.unsafeExtraThread

newCache :: forall k v. (Eq k, Hashable k) => (k -> Shake.Action v) -> Shake.Rules (k -> Shake.Action v)
newCache = Shake.newCache

historyDisable :: Shake.Action ()
historyDisable = Shake.historyDisable

produces :: [FilePath] -> Shake.Action ()
produces = Shake.produces

needHasChanged :: [FilePath] -> Shake.Action [FilePath]
needHasChanged = Shake.needHasChanged

resultHasChanged :: FilePath -> Shake.Action Bool
resultHasChanged = Shake.resultHasChanged

batch :: Int -> ((a -> Shake.Action ()) -> Shake.Rules ()) -> (a -> Shake.Action b) -> ([b] -> Shake.Action ()) -> Shake.Rules ()
batch = Shake.batch

reschedule :: Double -> Shake.Action ()
reschedule = Shake.reschedule

putLoud :: String -> Shake.Action ()
putLoud = Shake.putLoud

putNormal :: String -> Shake.Action ()
putNormal = Shake.putNormal

putQuiet :: String -> Shake.Action ()
putQuiet = Shake.putQuiet