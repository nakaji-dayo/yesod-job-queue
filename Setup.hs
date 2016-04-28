import Distribution.PackageDescription (PackageDescription(extraSrcFiles))
import Distribution.Simple (UserHooks(buildHook))
import qualified Distribution.Simple as DS
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Simple.Setup (BuildFlags)
import qualified System.Process as Proc
import qualified System.Directory as Dir
import Control.Monad (when, void)

buildHook' :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
buildHook' packageDesc localBuildInfo userHooks buildFlags = do
    exists <- Dir.doesFileExist "./app/dist/app.bundle.js"
    when (not exists) $ void $ Proc.system "cd app && npm install && npm run build-js"
    (buildHook DS.simpleUserHooks) packageDesc localBuildInfo userHooks buildFlags

main :: IO ()
main = DS.defaultMainWithHooks DS.simpleUserHooks{buildHook = buildHook'}
