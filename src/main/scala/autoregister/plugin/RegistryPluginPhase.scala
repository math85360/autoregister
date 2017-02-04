package autoregister.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.{ Global, Phase }
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.Transform

class RegistryPluginPhase(
  val global: Global,
  regs:       () => Map[Option[String], Set[String]],
  dones:      String => Unit
)
    extends PluginComponent with TypingTransformers with Transform { t =>

  import global._
  import global.definitions._

  val runsAfter = List("autoregister:inventory")

  override val runsBefore = List("autoregister:check")

  val phaseName = "autoregister:registry"

  override def newTransformer(unit: CompilationUnit) = {
    new RegistryTransformer(unit, regs())
  }

  class RegistryTransformer(unit: CompilationUnit, registries: Map[Option[String], Set[String]]) extends TypingTransformer(unit) {
    def fromRegistry(b: DefDef)(registryOpt: Set[String]) = registryOpt match {
      case Seq() => b
      case registry =>
        registry foreach { dones }
        val registers = registry map { s => q"register($s)" }
        b.rhs match {
          case Block(stats, expr) =>
            b.copy(rhs = Block(stats ++ registers, expr))
          case l @ Literal(_) =>
            b.copy(rhs = Block(List() ++ registers, l))
        }
    }

    override def transform(tree: Tree): Tree = super.transform(tree) match {
      case m @ ModuleDef(_, _, _) =>
        val updatedBody = m.impl.body.map {
          case b @ DefDef(_, tname, _, _, _, rhs) if "<init>" == tname.decoded =>
            fromRegistry(b) {
              m.impl.body flatMap {
                case d @ DefDef(_, _, _, _, _, _) => registries.get(Some(d.symbol.fullNameString)).getOrElse(Nil)
                case _                            => Nil
              } toSet
            }
          case b @ DefDef(_, _, _, _, _, rhs) =>
            m.symbol.getAnnotation(typeOf[autoregister.annotations.Registry].typeSymbol) match {
              case None => b
              case Some(_) =>
                fromRegistry(b) { registries.get(None).getOrElse(Nil).toSet }
            }
          case x => x
        }
        val r = m.copy(impl = m.impl.copy(body = updatedBody))
        r
      case _ => tree
    }
  }

}