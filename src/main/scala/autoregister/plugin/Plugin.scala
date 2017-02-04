package autoregister.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import scala.reflect.api.Positions

class RuntimePlugin(global: Global) extends TestPlugin(global)
class TestPlugin(
  val global: Global,
  reporter:   Seq[Value] => Unit = _ => ()
)
    extends Plugin {

  val name = "autoregister"

  val description = "Simplify the building of registry containing a set of specific objects"

  val registry = Registry()

  val components = List[PluginComponent](
    new InventoryPluginPhase(global, registry.+=),
    new RegistryPluginPhase(global, () => registry.result, registry.registered),
    new CheckPluginPhase(global, () => registry.checkRest)
  )

  case class ThrowError(cu: global.CompilationUnit, pos: global.Position, msg: String)
}