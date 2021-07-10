package autoregister.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.{ Global, Phase }
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.Transform
import Utils._

class InventoryPluginPhase(
  val global: Global,
  addToRegistry: Value => Unit)
  extends PluginComponent with TypingTransformers with Transform { t =>

  import global._
  import global.definitions._

  val runsAfter = List("typer")

  override val runsBefore = List("autoregister:registry")

  val phaseName = "autoregister:inventory"

  override def newTransformer(unit: CompilationUnit) = new InventoryTransformer(unit)

  val register = typeOf[autoregister.annotations.Register].typeSymbol
  val objRegister = typeOf[autoregister.annotations.RegisterAllDescendentObjects].typeSymbol
  val clsRegister = typeOf[autoregister.annotations.RegisterAllDescendentConcreteClasses].typeSymbol
  val cClsRegister = typeOf[autoregister.annotations.RegisterAllDescendentCaseClasses].typeSymbol

  class InventoryTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    implicit class RichString(s: String) {
      private val forbiddenPrefix = Seq("akka.", "scala.", "java.", "scalajs.")
      def allowed: Boolean = !forbiddenPrefix.exists(s.startsWith(_))
    }
    def getParents(child: Symbol) = {
      val r = child.parentSymbols.filter(_.fullNameString.allowed)
      // println(child.fullNameString + " : " + r.mkString(","))
      r
    }

    def process(symbol: Symbol) {
      val name = symbol.fullNameString
      val path = symbol.associatedFile.path
      /**
       * Add Value.ToRegister object for each annotation found in lst
       */
      def reg(lst: Symbol => List[Symbol], annotations: (Symbol, RegisteringType)*) {
        for {
          ancestor <- lst(symbol)
          (annotation, rtpe) <- annotations
          annot <- ancestor.getAnnotation(annotation)
        } {
          val r = Value.ToRegister(rtpe, name, path, annot.args.headOption collect {
            case Literal(Constant(s: String)) => s
          })
          addToRegistry(r)
        }
      }
      if (symbol.isModule) {
        reg(_.ancestors, objRegister -> RegisteringType.Object)
      } else {
        reg(_.ancestors, clsRegister -> RegisteringType.ConcreteClass, cClsRegister -> RegisteringType.CaseClass)
      }
      reg(_ :: Nil, register -> RegisteringType.Object)
    }

    override def transform(tree: Tree): Tree = tree match {
      case m @ ClassDef(_, _, _, _) =>
        if (m.symbol.isConcreteClass) process(m.symbol)
        else {}
        /**
         * Don't process any objects or classes inside classes.
         * They need outer class to be instantiated before.
         */
        tree
      case m @ ModuleDef(_, _, _) =>
        process(m.symbol)
        /**
         * Process any objects or classes inside objects.
         * Because they are instantiables directly.
         */
        super.transform(tree)
      case _@ PackageDef(_, _) =>
        /**
         * Process any packages
         */
        super.transform(tree)
      case _@ Template(_, _, _) =>
        super.transform(tree)
      case _ =>
        /**
         * Don't process other items because they are implementation or any
         * other codes
         */
        tree
    }
  }

}
