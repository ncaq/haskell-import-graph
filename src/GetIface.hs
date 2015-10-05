module GetIface where
import           BinIface
import           ClassyPrelude
import           GHC
import           System.Process
import           TcRnMonad

findIfaces :: Ghc [ModIface]
findIfaces = do
    his <- liftIO findHiFiles
    _ <- setSessionDynFlags =<< getSessionDynFlags
    mapM getIface his

findHiFiles :: IO [FilePath]
findHiFiles = lines <$> readProcess "find" ["-iname", "*.hi"] ""

getIface :: FilePath -> Ghc ModIface
getIface filename = do
    hsc_env <- getSession
    liftIO . initTcRnIf 's' hsc_env () () $ readBinIface IgnoreHiWay QuietBinIFaceReading filename
