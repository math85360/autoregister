package autoregister.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import scala.reflect.api.Positions

object RuntimeSettings {
  val registry = Registry()
}

class RuntimePlugin(global: Global) extends TestPlugin(global, RuntimeSettings.registry) {
  // Bad workaround
  // TODO Better implementation to serialize & deserialize registry content for incremental compilation
  override def init(options: List[String], error: String => Unit): Boolean = {
    registry.load(".cache_autoregister")
    super.init(options, error)
  }
}

class TestPlugin(
  val global:   Global,
  val registry: Registry                              = Registry(),
  reporter:     (Option[String], Set[String]) => Unit = { case _ => () }
)
    extends Plugin {

  val name = "autoregister"

  val description = "Simplify the building of registry containing a set of specific objects"

  val components = List[PluginComponent](
    new InventoryPluginPhase(global, registry.+=),
    new RegistryPluginPhase(global, () => registry.result, registry.registered, reporter),
    new CheckPluginPhase(global, () => { registry.save; registry.checkRest })
  )

  case class ThrowError(cu: global.CompilationUnit, pos: global.Position, msg: String)
}