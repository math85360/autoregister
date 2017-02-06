package autoregister.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.{ Global, Phase }
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.ast.TreeDSL

class RegistryPluginPhase(
  val global: Global,
  regs:       () => Map[Option[String], Set[String]],
  dones:      String => Unit,
  reporter:   (Option[String], Set[String]) => Unit
)
    extends PluginComponent with TypingTransformers with Transform with TreeDSL { t =>

  import global._
  import global.definitions._

  val runsAfter = List("autoregister:inventory")

  override val runsBefore = List("autoregister:check")

  val phaseName = "autoregister:registry"

  override def newTransformer(unit: CompilationUnit) = {
    new RegistryTransformer(unit, regs().withDefaultValue(Set.empty))
  }

  class RegistryTransformer(unit: CompilationUnit, registries: Map[Option[String], Set[String]]) extends TypingTransformer(unit) {
    def fromRegistry(m: ModuleDef)(registryOpt: Set[(String, String)]) = registryOpt match {
      case s if s.isEmpty => None
      case registry =>
        registry foreach { s => dones(s._2) }
        val registers = registry map { s =>
          import rootMirror.{ getModuleByName, getPackage, getClassByName, getRequiredClass, getRequiredModule, getClassIfDefined, getModuleIfDefined, getPackageObject, getPackageIfDefined, getPackageObjectIfDefined, requiredClass, requiredModule }
          import CODE._
          val app = getRequiredModule(s._2)
          val owner = m.symbol.tpe.typeSymbol
          val method = m.symbol.tpe.member(TermName(s._1))
          localTyper.typed { Apply(This(owner) DOT method, List(Ident(app))) }
        }
        Some(registers)
    }

    override def transform(tree: Tree): Tree = tree match {
      case m @ ModuleDef(_, _, _) =>
        val updatedBody = m.impl.body.map {
          case b @ DefDef(_, _, _, _, _, rhs) =>
            m.symbol.getAnnotation(typeOf[autoregister.annotations.Registry].typeSymbol) match {
              case None => b
              case Some(_) =>
                reporter(None, registries(None))
                fromRegistry(m) { registries(None).map(s => b.symbol.decodedName -> s) } match {
                  case Some(registers) =>
                    b.rhs match {
                      case Block(stats, expr) =>
                        treeCopy.Block(b, stats ++ registers, expr)
                      case expr @ Literal(_) =>
                        treeCopy.Block(b, List() ++ registers, expr)
                    }
                  case None =>
                    b
                }
            }
          case x => x
        }
        val r = (fromRegistry(m) {
          m.impl.body flatMap {
            case d @ DefDef(_, _, _, _, _, _) =>
              val key = Some(d.symbol.fullNameString)
              reporter(key, registries(key))
              registries(key).map(s => "register" -> s)
            case _ =>
              Nil
          } toSet
        } match {
          case Some(registers) if registers.nonEmpty =>
            treeCopy.ModuleDef(m, m.mods, m.name,
              treeCopy.Template(m.impl, m.impl.parents, m.impl.self, updatedBody ++ registers))
          case _ =>
            treeCopy.ModuleDef(m, m.mods, m.name,
              treeCopy.Template(m.impl, m.impl.parents, m.impl.self, updatedBody))
        })
        //println(r)
        super.transform(r)
      case _ => super.transform(tree)
    }
  }

}
