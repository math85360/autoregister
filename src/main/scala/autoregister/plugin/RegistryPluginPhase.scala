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

  sealed trait CallRegisterable[T] {
    def transform(in: T, registeringType: Option[Tree]): Tree
  }

  object CallRegisterable {
    implicit object ObjectCallRegisterable extends CallRegisterable[ModuleSymbol] {
      def transform(in: ModuleSymbol, registeringType: Option[Tree]) = Ident(in)
    }
    implicit object ClassCallRegisterable extends CallRegisterable[ClassSymbol] {
      def transform(in: ClassSymbol, registeringType: Option[Tree]) = TypeApply(Ident(TermName("classOf")), List(Ident(in)))
    }
  }

  class RegistryTransformer(unit: CompilationUnit, registries: Map[Option[String], Set[String]]) extends TypingTransformer(unit) {

    def mkRegisterMethodCall[T: CallRegisterable](owner: Symbol, registerMethod: Symbol, tpe: Option[Tree], toRegister: T) = {
      import CODE._
      val module = implicitly[CallRegisterable[T]].transform(toRegister, tpe)
      localTyper.typed {
        Apply(This(owner) DOT registerMethod, List(module))
      }
    }

    def fromRegistry(m: ModuleDef)(registry: Set[(String, String)]) = if (registry.isEmpty) None
    else {
      registry foreach { s => dones(s._2) }
      val registers =
        (for {
          s <- registry
          owner = m.symbol.tpe.typeSymbol
          registerMethod = m.symbol.tpe.member(TermName(s._1))
          member <- m.impl.body
          DefDef(_, tname, _, List(List(ValDef(_, _, tpe: TypeTree, _)), _*), _, _) <- member
          if tname.decoded == s._1
        } yield (for {
          AppliedTypeTree(mainTpe, _) <- tpe.original
          if mainTpe.symbol.decodedName == "Class"
        } yield {
          val classToRegister = rootMirror.getRequiredClass(s._2)
          mkRegisterMethodCall(owner, registerMethod, Some(mainTpe), classToRegister)
        }) match {
          case List(register) =>
            register
          case List() =>
            val moduleToRegister = rootMirror.getRequiredModule(s._2)
            mkRegisterMethodCall(owner, registerMethod, None, moduleToRegister)
        })
      Some(registers)
    }

    override def transform(tree: Tree): Tree = tree match {
      case m @ ModuleDef(_, _, _) =>
        val r = (fromRegistry(m) {
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
