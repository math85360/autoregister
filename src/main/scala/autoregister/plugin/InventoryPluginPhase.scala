package autoregister.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.{ Global, Phase }
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.Transform

class InventoryPluginPhase(
  val global:    Global,
  addToRegistry: Value => Unit
)
    extends PluginComponent with TypingTransformers with Transform { t =>

  import global._
  import global.definitions._

  val runsAfter = List("typer")

  override val runsBefore = List("patmat", "autoregister:registry")

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
          //unit.echo(ai.pos, r.prettyPrint)
          addToRegistry(r)
        case ai @ AnnotationInfo(`desRegister`, args, _) =>
          val r = Value.RegisterSuper(name, path, args.headOption collect {
            case Literal(Constant(s: String)) => s
          })
          //unit.echo(ai.pos, r.prettyPrint)
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
        //unit.echo(symbol.pos, r.prettyPrint)
        addToRegistry(r)
      }
    }

    override def transform(tree: Tree): Tree = super.transform(tree) match {
      case m @ ClassDef(_, _, _, _) =>
        t(m.symbol)
        m
      case m @ ModuleDef(_, _, _) =>
        t(m.symbol)
        m
      case _ => tree
    }
  }

}
