package autoregister.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.{ Global, Phase }
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.Transform
import Utils._

class InventoryPluginPhase(
  val global:    Global,
  addToRegistry: Value => Unit
)
    extends PluginComponent with TypingTransformers with Transform { t =>

  import global._
  import global.definitions._

  val runsAfter = List("typer")

  override val runsBefore = List("autoregister:registry")

  val phaseName = "autoregister:inventory"

  override def newTransformer(unit: CompilationUnit) = new InventoryTransformer(unit)

  val register = typeOf[autoregister.annotations.Register]
  val desRegister = typeOf[autoregister.annotations.RegisterAllDescendentObjects]

  class InventoryTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    implicit class RichString(s: String) {
      private val forbiddenPrefix = Seq("akka.", "scala.", "java.", "scalajs.")
      def allowed: Boolean = !forbiddenPrefix.exists(s.startsWith(_))
    }

    def t(symbol: Symbol) {
      val name = symbol.fullNameString
      val path = symbol.associatedFile.path
      symbol.annotations collect {
        case ai @ AnnotationInfo(`register`, args, assocs) =>
          val r = Value.ObjectToRegister(name, path, args.headOption collect {
            case Literal(Constant(s: String)) => s
          })
          addToRegistry(r)
        case ai @ AnnotationInfo(`desRegister`, args, _) =>
          val r = Value.RegisterSuper(name, path, args.headOption collect {
            case Literal(Constant(s: String)) => s
          })
          addToRegistry(r)
      }
      val parents =
        symbol.parentSymbols filter (_.fullNameString.allowed) map (_.fullNameString)
      if (parents.nonEmpty) {
        val r = (if (symbol.isModule) {
          Value.ConcreteObject(name, path, parents)
        }
        else {
          Value.Extends(name, path, parents)
        })
        addToRegistry(r)
      }
    }

    def getParents(child: Symbol) = {
      val r = child.parentSymbols.filter(_.fullNameString.allowed)
      // println(child.fullNameString + " : " + r.mkString(","))
      r
    }

    def t2(symbol: Symbol) {
      val name = symbol.fullNameString
      val path = symbol.associatedFile.path
      symbol.ancestors foreach { ancestor =>
        ancestor.getAnnotation(desRegister.typeSymbol) foreach { annot =>
          val r = Value.ObjectToRegister(name, path, annot.args.headOption collect {
            case Literal(Constant(s: String)) => s
          })
          addToRegistry(r)
        }
      }
      symbol.annotations collect {
        case ai @ AnnotationInfo(`register`, args, assocs) =>
          val r = Value.ObjectToRegister(name, path, args.headOption collect {
            case Literal(Constant(s: String)) => s
          })
          addToRegistry(r)
      }
    }

    override def transform(tree: Tree): Tree = super.transform(tree) match {
      /*case m @ ClassDef(_, _, _, _) =>
        t2(m.symbol)
        m*/
      case m @ ModuleDef(_, _, _) =>
        t2(m.symbol)
        m
      case _ => tree
    }
  }

}
