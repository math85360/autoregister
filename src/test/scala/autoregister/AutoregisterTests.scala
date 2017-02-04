package autoregister

import utest._
import TestUtils.make

object AutoregisterTests extends TestSuite {
  def tests = TestSuite {
    def st(_pkg: String, f: String, items: String*)(implicit toplevel: String) = {
      val pkg = (Option(toplevel).toSeq :+ _pkg) mkString "."
      make(if (f.isEmpty()) pkg.replace('.', '/') + "/" else (Option(toplevel.replace('.', '/')).toSeq :+ f) mkString "/") {
        (items map { item =>
          Option(s"$pkg.A.register") -> s"$pkg.$item"
        }) groupBy (_._1) mapValues (_.map(_._2).toSet) toMap
      }
    }

    'success{
      'samefile{
        implicit val t = "success.samefile"
        'simple - st("simple", "Simple.scala", "B")
        /*'descendent - st("descendent", "Descendent.scala", "B")
        'secondlevel - st("secondlevel", "SecondLevel.scala", "C")*/
      }
      /*'independent{
        implicit val t = "success.independent"
        'simple - st("simple", "", "B")
        'descendent - st("descendent", "", "B")
        'secondlevel - st("secondlevel", "", "C")
      }*/
    }
  }
}