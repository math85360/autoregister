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
    def t(symbol: Symbol) {
      val name = symbol.fullNameString
      val path = symbol.associatedFile.path
      symbol.annotations collect {
        case ai @ AnnotationInfo(`register`, args, assocs) =>
          addToRegistry(Value.ObjectToRegister(name, path, args.headOption collect { case Literal(Constant(s: String)) => s }))
        case AnnotationInfo(`desRegister`, args, _) =>
          addToRegistry(Value.RegisterDescendentOf(name, path, args.headOption collect { case Literal(Constant(s: String)) => s }))
      }
      val parents =
        symbol.parentSymbols filterNot (typeOf[Object].typeSymbol == _) map (_.fullNameString)
      if (symbol.isModule) addToRegistry(Value.ObjectMaybeToRegister(name, path, parents))
      else parents foreach (p => addToRegistry(Value.TraitInherits(name, path, p)))
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