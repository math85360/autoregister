package autoregister.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.{ Global, Phase }
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.Transform

class CheckPluginPhase(
  val global: Global,
  rest:       () => Set[String]
)
    extends PluginComponent with TypingTransformers with Transform { t =>

  import global._
  import global.definitions._

  val runsAfter = List("autoregister:registry")

  override val runsBefore = List("patmat")

  val phaseName = "autoregister:check"

  override def newTransformer(unit: CompilationUnit) = {
    new CheckTransformer(unit, rest())
  }

  class CheckTransformer(unit: CompilationUnit, registries: Set[String]) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = super.transform(tree) match {
      case m @ ModuleDef(_, _, _) =>
        m
      case _ => tree
    }
  }

}