package autoregister.plugin

import scala.annotation.tailrec
import scala.tools.nsc.Global

sealed trait Value {
  def prettyPrint: String
}

object Value {
  case class ObjectToRegister(name: String, path: String, registerTo: Option[String]) extends Value {
    override def prettyPrint = s"object $name will register to $registerTo [in file $path]"
  }
  case class RegisterSuper(name: String, path: String, registerTo: Option[String]) extends Value {
    override def prettyPrint = s"object descending from $name will register to $registerTo [in file $path]"
  }
  case class Extends(name: String, path: String, parents: List[String]) extends Value {
    override def prettyPrint = s"trait/class $name descending from $parents"
  }
  case class ConcreteObject(name: String, path: String, parents: List[String]) extends Value {
    override def prettyPrint = s"object $name descending from $parents"
  }
}

case class Registry() {
  private[plugin] var registries: Seq[(Option[String], String)] = Seq()
  private[plugin] var registeredSuper: Seq[Value.RegisterSuper] = Seq()
  private[plugin] var extended: Map[String, Seq[Value.Extends]] = Map().withDefaultValue(Seq())
  private[plugin] var concreteObjects: Map[String, Seq[Value.ConcreteObject]] = Map().withDefaultValue(Seq())
  private[plugin] var registeredObjects: Set[String] = Set()

  def checkRest: Set[String] = {
    concreteObjects.flatMap(_._2).map(_.name).toSet.diff(registeredObjects)
  }

  def registered(name: String): Unit = {
    registeredObjects += name
  }

  def +=(entry: Value): Unit = entry match {
    case e @ Value.ObjectToRegister(name, _, registerTo) => registries :+= registerTo -> name
    case e @ Value.RegisterSuper(_, _, _)                => registeredSuper :+= e
    case e @ Value.Extends(_, _, parents)                => parents foreach (name => extended += name -> (extended(name) :+ e))
    case e @ Value.ConcreteObject(_, _, parents)         => parents foreach (name => concreteObjects += name -> (concreteObjects(name) :+ e))
  }
  lazy val result: Map[Option[String], Set[String]] = {
    val r = (for {
      rsup <- registeredSuper
      ext <- allExtended(Nil, rsup.name)
      concrete <- { /*println(ext); */ concreteObjects(ext) }
    } yield {
      rsup.registerTo -> concrete.name
    }) ++ registries groupBy (_._1) mapValues (_.map(_._2).toSet)
    r
  }

  @tailrec
  private def allExtended(done: Seq[String], lst: String*): Seq[String] = lst match {
    case Seq() =>
      done
    case head +: tail =>
      allExtended(done :+ head, ((extended(head) map (_.name)) ++ tail): _*)
  }
}