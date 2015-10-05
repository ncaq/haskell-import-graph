module Main where
import           ClassyPrelude
import           Data.GraphViz
import           Data.GraphViz.Printing (renderDot)
import qualified Data.Text.Lazy         as L
import qualified Data.Text.Lazy.IO      as L
import           GetIface
import           GHC
import           ImportGraph
import           System.Process

main :: IO ()
main = do
    args <- getArgs
    libDir <- getLibDirReadProcess
    runGhc libDir $
        case args of
            ["-h"] -> liftIO $ L.putStrLn help
            ["--help"] -> liftIO $ L.putStrLn help
            [] -> do
                ifaces <- findIfaces
                liftIO . L.putStrLn $ renderGraph ifaces
            [hiPath] -> do
                iface <- getIface $ unpack hiPath
                liftIO . L.putStrLn $ renderGraph [iface]
            _ -> liftIO $ L.putStrLn help

renderGraph :: [ModIface] -> L.Text
renderGraph = renderDot . toDot . importGraph (Str "haskell-import-graph")

getLibDirReadProcess :: IO (Maybe FilePath)
getLibDirReadProcess = listToMaybe . lines <$> readProcess "ghc" ["--print-libdir"] ""

help :: L.Text
help = unlines [ "Usage: cabal build && haskell-import-graph"
               , "or:    cabal build && haskell-import-graph dist/build/foo/foo-tmp/bar.hi"
               ]
