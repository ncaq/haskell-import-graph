module System.ImportGraph.ModuleCluster where
import           Avail
import           ClassyPrelude
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Types.Monadic
import qualified Data.Text.Lazy                    as L
import           HscTypes
import           Module
import           Name

moduleCluster :: ModIface -> Dot L.Text
moduleCluster iface@ModIface{..} = do
    cluster (Str (ifaceName iface)) $ do
        graphAttrs [textLabel (ifaceName iface)]
        node (ifaceDummyNodeName iface) [Style [SItem Invisible []]]
        mapM node' exports
    mapM_ (ifaceUsageEdges iface) mi_usages
  where exports = map (pack . getOccString . availName) mi_exports

ifaceName :: ModIface -> L.Text
ifaceName = moduleText . mi_module

moduleText :: Module -> L.Text
moduleText = pack . moduleNameString . moduleName

ifaceDummyNodeName :: ModIface -> L.Text
ifaceDummyNodeName iface = ifaceName iface <> "_dummy_node"

ifaceClusterName :: ModIface -> L.Text
ifaceClusterName iface = "cluster_" <> ifaceName iface

ifaceUsageEdges :: ModIface -> Usage -> Dot L.Text
ifaceUsageEdges iface UsagePackageModule{..} = do
    ifaceEdge iface (moduleText usg_mod)
    node (moduleText usg_mod) [Shape Component]
ifaceUsageEdges iface UsageHomeModule{..}    =
    mapM_ (ifaceEdge iface . pack . occNameString . fst) usg_entities
ifaceUsageEdges iface UsageFile{..}          =
    ifaceEdge iface (pack usg_file_path)

ifaceEdge :: ModIface -> L.Text -> Dot L.Text
ifaceEdge iface edgeTo = edge (ifaceDummyNodeName iface) edgeTo [LTail (ifaceClusterName iface)]
