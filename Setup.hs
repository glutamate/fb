import Distribution.Simple
import System.Process
import System.Environment (getArgs)
import System.IO (hFlush, hPutStrLn, stderr)

main = do

  -- there are more complicated hooks to put things
  -- into the build process, and maybe those could
  -- be used rather than generating the bindings
  -- over and over at every opportunity.
  args <- getArgs
  hPutStrLn stderr $ "Setup.hs for fb bindings: " ++ show args
  hFlush stderr
  callCommand "cd gen && stack build && stack exec gen-exe"
  defaultMain


