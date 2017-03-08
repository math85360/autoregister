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
  regs:       () => Map[Option[String], Set[Value.ToRegister]],
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
    new RegistryTransformer(unit, regs())
  }

  class RegistryTransformer(unit: CompilationUnit, registries: Map[Option[String], Set[Value.ToRegister]]) extends TypingTransformer(unit) {

    sealed trait CallRegisterable[T] {
      def transform(in: T, registeringType: Option[Tree]): Tree
    }

    def prettyPrint(s: String): String = {
      @tailrec
      def loop(i: Int, done: String, nl: Boolean)(implicit tabs: Int): String =
        if (i >= s.length) done
        else {
          val start = if (nl) done + "\n" + "  " * tabs else done
          def eolc(cnt: Int = 0) = "\n" + "  " * (tabs + cnt)
          def eol = eolc()
          s.charAt(i) match {
            case '(' =>
              s.indexWhere(c => "(,)".contains(c), i + 1) match {
                case x if x != -1 && s.charAt(x) == ')' => loop(i + 1, s"$start(", false)(tabs + 1)
                case _                                  => loop(i + 1, s"$start(", true)(tabs + 1)
              }
            case ')'  => loop(i + 1, s"$start)", true)(tabs - 1)
            case ','  => loop(i + 1, s"$done,", true)
            case '\n' => loop(i + 1, s"$start", true)
            case ' '  => loop(i + 1, start, false)
            case x    => loop(i + 1, s"$start$x", false)
          }
        }
      loop(0, "", false)(0)
    }

    object OriginalTypeTree {
      def unapply(tpe: TypeTree): Option[Tree] =
        if (!tpe.original.isEmpty) Some(tpe.original)
        else None
    }

    def fromRegistry(m: ModuleDef)(registry: Set[(String, Value.ToRegister)]): Option[Set[Tree]] = if (registry.isEmpty) None
    else {
      import CODE._
      registry foreach { s => dones(s._2.name) }
      val registers =
        (for {
          s <- registry
          owner = m.symbol.tpe.typeSymbol
          registerMethod = m.symbol.tpe.member(TermName(s._1))
          member <- m.impl.body
          DefDef(_, tname, _, List(args, _*), _, _) <- member
          if tname.decoded == s._1
        } yield {
          val res: Tree =
            s._2.tpe match {
              case RegisteringType.CaseClass =>
                args match {
                  case List(ValDef(_, _, _: TypeTree, _), ValDef(_, _, _, _), ValDef(_, _, _, _)) =>
                    val List(ValDef(_, _, _: TypeTree, _), ValDef(_, _, _, _), ValDef(_, _, _, _)) = args
                    val cls = rootMirror.getRequiredClass(s._2.name)
                    val mod = cls.companionModule
                    /*val fields = cls.tpe.decls.collect {
                      case s: TermSymbol if s.isVal && s.isCaseAccessor =>
                        (TermName(treeBuilder.fresh.newName(s.name.toString.trim)), s.typeSignature)
                    }
                    //val app = TermName(s"app${cls.fullNameAsName('_')}")
                    //val appargs: List[Tree] = (for { (name, symbol) <- fields } yield q"val $name:$symbol").toList
                    //treeBuilder.makeFunctionTypeTree(appargs, treeBuilder.)
                    //val appargsv = for { (name, _) <- fields } yield Ident(name.decoded)
                    //val appdef = q"""private def $app(..$appargs):$cls = $mod.apply(..$appargsv)"""
                    fields.size match {
                      case 0 =>
                        q"""
                            $registerMethod(classOf[$cls], (_: Unit) => $mod.apply(), (_:$cls) => ())
                          """
                      case 1 =>
                        q"""
                            $registerMethod(classOf[$cls], ($mod.apply _), ($mod.unapply _).andThen(_.get))
                          """
                      case _ =>
                        q"""
                            $registerMethod(classOf[$cls], ($mod.apply _).tupled, ($mod.unapply _).andThen(_.get))
                          """
                    }*/
                    //val tpe = mod
                    val tpes = cls.tpe.decls.collect {
                      case s: TermSymbol if s.isVal && s.isCaseAccessor => s.typeSignature
                    }
                    val fname = TermName(s"fromTuple${cls.fullNameAsName('_')}")
                    val tname = TermName(s"toTuple${cls.fullNameAsName('_')}")
                    tpes match {
                      case Nil =>
                        /*val $fname: (Unit) => $cls = (_:Unit) => $mod.apply()
                          val $tname: ($cls) => Unit = _ => ()*/
                        q"""
                          
                          $registerMethod(classOf[$cls], (_:Unit) => $mod.apply(), (_:$cls) => ())
                          """
                      //q""" () """
                      case head :: Nil =>
                        /*q"""val $fname: ($head) => $cls = ($mod.apply _)""",
                          q"""val $tname: ($cls) => (..$tpes) = ($mod.unapply
                          _).andThen(_.get)""",*/
                        q"""$registerMethod(classOf[$cls], ($mod.apply _), ($mod.unapply _).andThen(_.get))"""
                      //q""" () """
                      case _ =>
                        /*q"""val $fname: ((..$tpes)) => $cls = ($mod.apply _).tupled""",
                          q"""val $tname: ($cls) => (..$tpes) = """,*/
                        q"""$registerMethod(classOf[$cls], ($mod.apply _).tupled, ($mod.unapply _).andThen(_.get))"""
                      //
                    }
                  //q""" val $fname : $cls = null """
                  //println(defs.mkString("\n"))
                  //Block(defs: _*)
                  //q""" $registerMethod(classOf[$cls], $mod.fromTuple, $mod.toTuple) """
                  case _ =>
                    abort(s"${registerMethod.fullNameString} must have 3 args : (Class[C], T => C, C => T)")
                }
              case RegisteringType.ConcreteClass =>
                args match {
                  case List(ValDef(_, _, OriginalTypeTree(AppliedTypeTree(mainTpe, _)), _)) if mainTpe.symbol.decodedName == "Class" =>
                    val cls = rootMirror.getRequiredClass(s._2.name)
                    q"""$registerMethod(classOf[$cls])"""
                  case _ =>
                    abort(s"${registerMethod.fullNameString} must have 1 arg : (Class[C])")
                }
              case RegisteringType.Object =>
                args match {
                  case List(ValDef(_, _, _, _)) =>
                    val mod = rootMirror.getRequiredModule(s._2.name)
                    q"""$registerMethod($mod)"""
                  case _ =>
                    abort(s"${registerMethod.fullNameString} must have 1 arg")
                }
            }
          val typed = localTyper.atOwner(m.impl.symbol).typed(res)
          typed
        })
      Some(registers)
    }

    override def transform(tree: Tree): Tree = tree match {
      case m @ ModuleDef(_, _, _) =>
        val r = (fromRegistry(m) {
          m.impl.body flatMap {
            case d @ DefDef(_, tname, _, _, _, _) =>
              def process(key: Option[String]) = {
                reporter(key, registries(key).map(_.name))
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
            val z = treeCopy.ModuleDef(m, m.mods, m.name,
              treeCopy.Template(m.impl, m.impl.parents, m.impl.self, m.impl.body.map(transform) ++ registers))
            //println(registers.mkString("\n"))
            //println(showCode(z))
            //localTyper.typed(z)
            //localTyper.atOwner(m.symbol).typed(z)
            z
          case _ =>
            m
        })
        r
      case _@ PackageDef(_, _) =>
        super.transform(tree)
      case _@ Template(_, _, _) =>
        super.transform(tree)
      case _ =>
        tree
    }
  }

}
