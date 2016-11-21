{-# Language CPP #-}
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription (emptyHookedBuildInfo, FlagName (..))
import System.Process
import System.Environment (getArgs)
import System.IO (hFlush, hPutStrLn, stderr)

main = do
  defaultMainWithHooks hooks

hooks = simpleUserHooks {
    buildHook = optionallyRegenerate (buildHook simpleUserHooks)
  }

optionallyRegenerate cont packageDescription localBuildInfo userHooks buildFlags = do
  let flags = configConfigurationsFlags $ configFlags localBuildInfo
  let flag = lookup (FlagName "regenerate") flags
  case flag of
    Just True -> regenerate
    _ -> progress "Not regenerating"

  cont packageDescription localBuildInfo userHooks buildFlags
  return ()
  -- return emptyHookedBuildInfo -- TODO: this should return info that causes regeneration to happen approprately?

regenerate = do
  progress "Regenerating from gen/"
  callCommand "cd gen && stack build && stack exec gen-exe"
  progress "Regeneration done"

progress s = do
  hPutStrLn stderr s
  hFlush stderr
