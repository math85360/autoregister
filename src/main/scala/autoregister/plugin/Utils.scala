package autoregister.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.{ Global, Phase }
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.ast.TreeDSL
import scala.annotation.tailrec

object Utils {
  implicit class RichAny[A](root: A) {
    def treeCollectFirst[C](traverser: A => List[A], retain: A => Option[C]) = {
      @tailrec
      def loop(nodes: Seq[A]): Option[(A, C)] = nodes match {
        case Seq() => None
        case Seq(head, tail @ _*) =>
          retain(head) match {
            case None    => loop(traverser(head) ++ tail)
            case Some(r) => Some((head, r))
          }
      }
      loop(Seq(root))
    }
    def treeFlatMap[C](traverser: A => List[A], flattener: A => List[C]) = {
      @tailrec
      def loop(result: List[A], nodes: Seq[A]): List[A] = nodes match {
        case Seq() => result
        case Seq(head, tail @ _*) =>
          traverser(head) match {
            case Nil =>
              loop(result :+ head, tail)
            case lst =>
              loop(result, lst ++ tail)
          }
      }
      val r =
        loop(Nil, Seq(root))
      //println(r)
      r.flatMap(flattener)
    }
  }
}