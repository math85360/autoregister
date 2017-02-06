package autoregister

import utest._
import TestUtils.make

object AutoregisterTests extends TestSuite {
  def tests = TestSuite {

    'success{
      def s(_pkg: String, file: String)(items: Map[Option[String], Set[String]])(implicit toplevel: String) = {
        val pkg = s"$toplevel.${_pkg}"
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
      'globalregistry{
        implicit val t = "success"
        'global - s("globalregistry", "") { Map(None -> Set("globalregistry.C")) }
      }
    }
  }
}