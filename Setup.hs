import Distribution.Simple
import System.Exit (ExitCode (..))
import System.Process (waitForProcess, runProcess)
import System.Directory (getAppUserDataDirectory)

main = defaultMainWithHooks $ simpleUserHooks {postInst = buildForHaste}

-- Attempt to install library for use with haste-compiler
buildForHaste _ _ _ _ = do
  cabalDir <- getAppUserDataDirectory "cabal"
  let localHastec = cabalDir ++ "/bin/hastec"
  buildWith ["hastec", localHastec]

buildWith (hastec:hs) = do
  build <- runProcess hastec
                      ["-O2", "-Wall", "--libinstall", "FRP.Fursuit",
                       "FRP.Fursuit.Async"]
                      (Just "./src")
                      Nothing
                      Nothing
                      Nothing
                      Nothing
  res <- waitForProcess build
  case res of
    ExitFailure _ -> buildWith hs
    _             -> return ()
buildWith _ =
  return ()
