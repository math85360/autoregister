package autoregister.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.{ Global, Phase }
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.Transform

class RegistryPluginPhase(
  val global: Global,
  regs:       () => Map[Option[String], Set[String]],
  dones:      String => Unit,
  reporter:   (Option[String], Set[String]) => Unit
)
    extends PluginComponent with TypingTransformers with Transform { t =>

  import global._
  import global.definitions._

  val runsAfter = List("autoregister:inventory")

  override val runsBefore = List("autoregister:check")

  val phaseName = "autoregister:registry"

  override def newTransformer(unit: CompilationUnit) = {
    new RegistryTransformer(unit, regs().withDefaultValue(Set.empty))
  }

  class RegistryTransformer(unit: CompilationUnit, registries: Map[Option[String], Set[String]]) extends TypingTransformer(unit) {
    def fromRegistry(m: ModuleDef, b: Option[DefDef])(registryOpt: Set[String]) = registryOpt match {
      case s if s.isEmpty => None
      case registry =>
        registry foreach { dones }
        //unit.warning(b.pos, "register : \n" + registry.mkString("[", ",", "]"))
        val registers = registry map { s =>
          val lastDot = s.split('.')
          //val tpe = s.take(lastDot)
          //val meth = s.drop(lastDot + 1)
          //Apply(Select(This(m.symbol), TypeName("register")))
          val app = lastDot match {
            case Array(head, tail @ _*) =>
              tail.foldLeft[Tree](Ident(head)) { (acc, cur) =>
                Select(acc, cur)
              }
          }
          //m.impl.body.collectFirst {
          //case x @ DefDef(_, _, _, _, _, _) if x.symbol.decodedName == "register" =>
          //treeBuild.mkAttributedThis(sym)
          val tpe = treeBuild.mkAttributedThis(m.symbol)
          /*val target = //Select(This(m.symbol), "register")
            treeBuild.mkAttributedSelect(, Ident(TermName("register")).symbol)*/
          val target = q"$tpe.register"
          /*import scala.tools.reflect.ToolBox
          val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
          showRaw(tb.parse(""))*/
          //treeBuild.mkMethodCall(target = target, List(app))
          //treeBuild.mkMethodCall(target = target, List(Ident(lastDot.last)))
          /*println(showRaw(treeBuild.mkRuntimeUniverseRef.find(_.symbol.fullNameString == s)))
          val t = (for {
            unit <- global.currentRun.units
            p <- unit.body.collect {
              case x @ ModuleDef(_, _, _) if x.symbol.fullNameString == s => x
            }
          } yield p)
          println(t.map(showRaw(_)))*/
          val u = Apply(Select(This(TypeName(m.symbol.decodedName)), TermName("register")), List(Ident(s)))
          println(showRaw(u))
          u
          //}
          /*val tpe = TypeName(m.symbol.decodedName)
          val meth = q"$tpe.this.register"
          val i = Ident(s)
          q"$meth($i)"*/
          //Apply(meth.toSymbol, Ident(s).to)
        } // map { s => q"register($s)" }
        /*val r = b.rhs match {
          case Block(stats, expr) =>
            b.copy(rhs = Block(stats ++ registers, expr))
          case l @ Literal(_) =>
            b.copy(rhs = Block(List() ++ registers, l))
        }*/
        //unit.echo(b.pos, r.toString())
        Some(registers)
    }

    override def transform(tree: Tree): Tree = tree match {
      case m @ ModuleDef(_, _, _) =>
        val updatedBody = m.impl.body.map {
          /*case b @ DefDef(_, tname, _, _, _, rhs) if "<init>" == tname.decoded =>
            fromRegistry(m, b) {
              m.impl.body flatMap {
                case d @ DefDef(_, _, _, _, _, _) =>
                  val key = Some(d.symbol.fullNameString)
                  reporter(key, registries(key))
                  registries(key)
                case _ =>
                  Nil
              } toSet
            }*/
          case b @ DefDef(_, _, _, _, _, rhs) =>
            m.symbol.getAnnotation(typeOf[autoregister.annotations.Registry].typeSymbol) match {
              case None => b
              case Some(_) =>
                reporter(None, registries(None))
                fromRegistry(m, Some(b)) { registries(None) } match {
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
          case x => println(showRaw(x)); x
        }
        val r = (fromRegistry(m, None) {
          m.impl.body flatMap {
            case d @ DefDef(_, _, _, _, _, _) =>
              val key = Some(d.symbol.fullNameString)
              reporter(key, registries(key))
              registries(key)
            case _ =>
              Nil
          } toSet
        } match {
          case Some(registers) if registers.nonEmpty =>
            val x = treeCopy.ModuleDef(m, m.mods, m.name,
              treeCopy.Template(m.impl, m.impl.parents, m.impl.self, updatedBody ++ registers))
            x
          case _ =>
            treeCopy.ModuleDef(m, m.mods, m.name,
              treeCopy.Template(m.impl, m.impl.parents, m.impl.self, updatedBody))
          //r.copy(impl = r.impl.copy(body = r.impl.body ++ registers))
        })
        //println(r)
        r
      case _ => super.transform(tree)
    }
  }

}