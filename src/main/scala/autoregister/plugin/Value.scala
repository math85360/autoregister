package autoregister.plugin

import scala.annotation.tailrec

sealed trait Value {
  def prettyPrint: String
}

object Value {
  case class ObjectToRegister(name: String, path: String, registerTo: Option[String]) extends Value {
    override def prettyPrint = s"object $name will register to $registerTo [in file $path]"
  }
  case class RegisterDescendentOf(name: String, path: String, registerTo: Option[String]) extends Value {
    override def prettyPrint = s"object descending from $name will register to $registerTo [in file $path]"
  }
  case class TraitInherits(name: String, path: String, from: String) extends Value {
    override def prettyPrint = s"trait/class $name descending from $from"
  }
  case class ObjectMaybeToRegister(name: String, path: String, parents: List[String]) extends Value {
    override def prettyPrint = s"object $name descending from $parents"
  }
}

case class Registry() {
  private[plugin] var registries: Seq[(Option[String], String)] = Seq()
  private[plugin] var descendentOf: Map[String, Value.RegisterDescendentOf] = Map()
  private[plugin] var inheritings: Map[String, Value.TraitInherits] = Map()
  private[plugin] var concreteObjects: Set[Value.ObjectMaybeToRegister] = Set()
  private[plugin] var registeredObjects: Set[String] = Set()

  def checkRest: Set[String] = {
    concreteObjects.map(_.name).diff(registeredObjects)
  }

  def registered(name: String): Unit = {
    registeredObjects += name
  }

  def +=(entry: Value): Unit = entry match {
    case e @ Value.ObjectToRegister(name, _, registerTo) => registries :+= registerTo -> name
    case e @ Value.RegisterDescendentOf(name, _, _)      => descendentOf += name -> e
    case e @ Value.TraitInherits(name, _, _)             => inheritings += name -> e
    case e @ Value.ObjectMaybeToRegister(_, _, parents)  => concreteObjects += e
  }
  lazy val result: Map[Option[String], Set[String]] = {
    (for {
      concreteObject <- concreteObjects
      parent <- concreteObject.parents
      registry <- findDescendentOf(parent)
    } yield {
      registry.registerTo -> concreteObject.name
    }) ++ registries groupBy (_._1) mapValues (_.map(_._2))
  }

  @tailrec
  private def findDescendentOf(name: String): Option[Value.RegisterDescendentOf] = {
    descendentOf.get(name) match {
      case None =>
        inheritings.get(name) match {
          case None    => None
          case Some(i) => findDescendentOf(i.from)
        }
      case Some(i) => Some(i)
    }
  }
}