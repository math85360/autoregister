package autoregister

import utest._
import TestUtils.make

object AutoregisterTests extends TestSuite {
  def tests = TestSuite {

    'success{
      def s(_pkg: String, file: String)(items: Map[Option[String], Set[String]])(implicit toplevel: String) = {
        val pkg = (if (toplevel.nonEmpty && _pkg.nonEmpty) s"$toplevel.${_pkg}" else s"$toplevel${_pkg}")
        make(file match {
          case _ if file.isEmpty() => pkg.replace('.', '/') + "/"
          case _                   => toplevel.replace('.', '/') + "/" + file
        })(items mapValues (_.map(toplevel + "." + _)))
        //else (Option(toplevel.replace('.', '/')).toSeq :+ f) mkString "/")
      }
      def st(pkg: String, f: String, items: String*)(implicit toplevel: String) = {
        s(pkg, f) {
          (items map { item =>
            Option(s"$toplevel.$pkg.A.register") -> s"$pkg.$item"
          }) groupBy (_._1) mapValues (_.map(_._2).toSet) toMap
        }
      }

      'samefile{
        implicit val t = "success.samefile"
        'simple - st("simple", "Simple.scala", "B")
        'descendent - st("descendent", "Descendent.scala", "B")
        'secondlevel - st("secondlevel", "SecondLevel.scala", "C")
      }
      'independent{
        implicit val t = "success.independent"
        'simple - st("simple", "", "B")
        'descendent - st("descendent", "", "B")
        'secondlevel - st("secondlevel", "", "C")
      }
      'generics{
        implicit val t = "success.generics"
        s("", "") { Map(None -> Set("C")) }
      }
      'globalregistry{
        implicit val t = "success.globalregistry"
        s("", "") { Map(None -> Set("C")) }
      }
      'withimplicits{
        implicit val t = "success.withimplicits"
        s("", "") { Map(None -> Set("ToRegister")) }
      }
      'classes{
        implicit val t = "success.classes"
        s("", "") { Map(None -> Set("C", "D", "F")) }
      }
      'caseclasses{
        implicit val t = "success.caseclasses"
        s("", "") { Map(None -> Set("C", "D", "E", "G", "J.K")) }
      }
      /*'inheritedregistry{
        implicit val t = "success.inheritedregistry"
        s("", "") { Map(None -> Set("A.register")) }
      }*/
    }
  }
}