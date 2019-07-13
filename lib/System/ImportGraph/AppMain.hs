{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.ImportGraph.AppMain where
import           ClassyPrelude
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Printing            (renderDot)
import qualified Data.GraphViz.Types.Generalised   as G
import           Data.GraphViz.Types.Monadic
import qualified Data.Text.Lazy                    as TL
import qualified Data.Text.Lazy.IO                 as TL
import           GHC
import           System.ImportGraph.GetIface
import           System.ImportGraph.ModuleCluster
import           System.Process

appMain :: IO ()
appMain = do
  args <- getArgs
  libDir <- getLibDirReadProcess
  runGhc libDir $
    case args of
      ["-h"] -> liftIO $ TL.putStrLn help
      ["--help"] -> liftIO $ TL.putStrLn help
      [] -> do
        ifaces <- findIfaces
        liftIO . TL.putStrLn $ renderGraph ifaces
      [hiPath] -> do
        iface <- getIface $ unpack hiPath
        liftIO . TL.putStrLn $ renderGraph [iface]
      _ -> liftIO $ TL.putStrLn help

getLibDirReadProcess :: IO (Maybe FilePath)
getLibDirReadProcess = listToMaybe . lines <$> readProcess "ghc" ["--print-libdir"] ""

help :: TL.Text
help = "Usage: haskell-import-graph"

renderGraph :: [ModIface] -> TL.Text
renderGraph = renderDot . toDot . importGraph (Str "haskell-import-graph")

importGraph :: GraphID -> [ModIface] -> G.DotGraph TL.Text
importGraph graphName mods = digraph graphName $ do
  graphAttrs [Compound True, RankDir FromLeft]
  mapM moduleCluster mods
