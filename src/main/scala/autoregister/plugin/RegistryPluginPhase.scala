package autoregister.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.{ Global, Phase }
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.ast.TreeDSL
import scala.annotation.tailrec
import Utils._

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

    def mkRegisterMethodCall(owner: Symbol, registerMethod: Symbol, moduleToRegister: ModuleSymbol, moduleInheritedType: Option[Symbol]) = {
      import rootMirror.{ getRequiredModule }
      import CODE._
      val module = Ident(moduleToRegister)
      localTyper.typed {
        Apply(This(owner) DOT registerMethod, List(module))
      }
    }

    def fromRegistry(m: ModuleDef)(registryOpt: Set[(String, String)]) = registryOpt match {
      case s if s.isEmpty => None
      case registry =>
        registry foreach { s => dones(s._2) }
        val registers = registry map { s =>
          import rootMirror.{ getRequiredModule }
          import CODE._
          val moduleToRegister = getRequiredModule(s._2)
          val owner = m.symbol.tpe.typeSymbol
          val registerMethod = m.symbol.tpe.member(TermName(s._1))
          val mod = localTyper.typed { Ident(moduleToRegister) }
          mod.symbol.ancestors flatMap { ancestor =>
            val annot = ancestor.getAnnotation(typeOf[autoregister.annotations.RegisterAllDescendentObjects].typeSymbol)
            annot map { annot => ancestor -> annot }
          }
          mod.symbol.treeCollectFirst(
            (_: Symbol).parentSymbols,
            (_: Symbol).getAnnotation(typeOf[autoregister.annotations.RegisterAllDescendentObjects].typeSymbol)
          ) match {
              case None =>
                mkRegisterMethodCall(owner, registerMethod, moduleToRegister, None)
              case Some((s, a)) =>
                mkRegisterMethodCall(owner, registerMethod, moduleToRegister, Some(s))
            }
        }
        Some(registers)
    }

    override def transform(tree: Tree): Tree = tree match {
      case m @ ModuleDef(_, _, _) =>
        val r = (fromRegistry(m) {
          /* When we've an object extending a class that have a register method
           * or an Registry annotation, we must call this method even if defined
           * in his ancestors
          println {
            val o =
              m.impl.treeFlatMap((_: Template).parents.collect {
                case cls: ClassDef  => cls.symbol.implClass
              }, (_: Template).body.collect({
                case d @ DefDef(_, tname, _, _, _, _) if tname.decoded == "register" || d.symbol.hasAnnotation(typeOf[autoregister.annotations.Registry].typeSymbol) =>
                  //d.symbol.getAnnotation(typeOf[autoregister.annotations.Registry].typeSymbol)
                  d.symbol.fullNameString
                case x => x.symbol.fullNameString
              }))
            o
          }*/
          m.impl.body flatMap {
            case d @ DefDef(_, tname, _, _, _, _) =>
              def process(key: Option[String]) = {
                reporter(key, registries(key))
                registries(key).map(s => tname.decoded -> s)
              }
              val registryAnnot = m.symbol.getAnnotation(typeOf[autoregister.annotations.Registry].typeSymbol).toSeq.flatMap { annot =>
                process(annot.args.headOption collect {
                  case Literal(Constant(key: String)) => key
                })
              }
              val registryOpt = if (tname.decoded == "register") {
                process(None)
              }
              else {
                Set()
              }
              val meth = process(Some(s"${m.symbol.fullNameString}.${tname.decoded}"))
              (meth ++ registryAnnot ++ registryOpt)
            case _ =>
              Nil
          } toSet
        } match {
          case Some(registers) if registers.nonEmpty =>
            treeCopy.ModuleDef(m, m.mods, m.name,
              treeCopy.Template(m.impl, m.impl.parents, m.impl.self, m.impl.body ++ registers))
          case _ =>
            m
        })
        super.transform(r)
      case _ => super.transform(tree)
    }
  }

}
