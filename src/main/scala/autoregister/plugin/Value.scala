package autoregister.plugin

import scala.annotation.tailrec
import scala.tools.nsc.Global
import scala.io.Source
import java.io.FileInputStream
import java.nio.file.Files
import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream
import java.io.ObjectInputStream
import java.nio.file.Path
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import java.io.ObjectStreamClass
import java.io.InputStream

sealed trait Value {
  def prettyPrint: String
}

sealed trait RegisterValue extends Value {
  def name: String
  def registerTo: Option[String]
}
sealed trait RegisteringType
object RegisteringType {
  case object CaseClass extends RegisteringType
  case object ConcreteClass extends RegisteringType
  case object Object extends RegisteringType
}

object Value {
  case class ToRegister(tpe: RegisteringType, name: String, path: String, registerTo: Option[String]) extends Value {
    override def prettyPrint = s"$tpe $name will register to $registerTo [in file $path]"
  }
  /*case class RegisterObjects(override val name: String, path: String, override val registerTo: Option[String]) extends RegisterValue {
    override def prettyPrint = s"object descending from $name will register to $registerTo [in file $path]"
  }*/
  /*case class RegisterConcreteClasses(override val name: String, path: String, override val registerTo: Option[String]) extends RegisterValue {
    override def prettyPrint = s"final class descending from $name will register to $registerTo [in file $path]"
  }*/
  /*case class Extends(name: String, path: String, parents: List[String]) extends Value {
    override def prettyPrint = s"trait/class $name descending from $parents"
  }*/
  /*case class ConcreteObject(name: String, path: String, parents: List[String]) extends Value {
    override def prettyPrint = s"object $name descending from $parents"
  }*/
}

object Registry {

}

case class Data(
  private[plugin] var registries: Map[Option[String], Set[Value.ToRegister]] = Map().withDefaultValue(Set.empty),
  //private[plugin] var registeredSuper:   Seq[RegisterValue]                         = Seq(),
  //private[plugin] var extended:          Map[String, Seq[Value.Extends]]            = Map().withDefaultValue(Seq()),
  //private[plugin] var concreteObjects:   Map[String, Seq[Value.ConcreteObject]] = Map().withDefaultValue(Seq()),
  private[plugin] var registeredObjects: Set[String] = Set())

class ScalaObjectInputStream(is: InputStream) extends ObjectInputStream(is) {
  override def resolveClass(desc: ObjectStreamClass): Class[_] = {
    try {
      Class.forName(desc.getName, false, getClass.getClassLoader)
    } catch {
      case ex: ClassNotFoundException => super.resolveClass(desc)
    }
  }
}

case class Registry() {

  private var data: Data = Data()
  private var target: Option[String] = None

  def load(path: String) {
    target = Some(path)
    if (Files.exists(java.nio.file.Paths.get(path))) {
      val fis = new FileInputStream(path)
      val ois = new ScalaObjectInputStream(fis)
      data = ois.readObject().asInstanceOf[Data]
      ois.close()
      fis.close()
    }
  }

  def save() {
    target foreach { path =>
      val fos = new FileOutputStream(path)
      val oos = new ObjectOutputStream(fos)
      oos.writeObject(data)
      oos.close()
      fos.close()
    }
  }

  def checkRest: Set[String] = {
    Set.empty
    //data.concreteObjects.flatMap(_._2).map(_.name).toSet.diff(data.registeredObjects)
  }

  def registered(name: String): Unit = {
    data.registeredObjects += name
  }

  def +=(entry: Value): Unit = entry match {
    case e @ Value.ToRegister(_, name, _, registerTo) => data.registries = data.registries.updated(registerTo, data.registries(registerTo) + e)
    //case e @ Value.ObjectToRegister(name, _, registerTo)        => data.registries :+= registerTo -> name
    //case e @ Value.ConcreteClassToRegister(name, _, registerTo) => data.registries :+= registerTo -> name
    //case e: RegisterValue                             => data.registeredSuper :+= e
    //case e @ Value.Extends(_, _, parents)             => parents foreach (name => data.extended += name -> (data.extended(name) :+ e))
    //case e @ Value.ConcreteObject(_, _, parents)      => parents foreach (name => data.concreteObjects += name -> (data.concreteObjects(name) :+ e))
  }
  lazy val result: Map[Option[String], Set[Value.ToRegister]] = {
    data.registries
    /*val r = (for {
      rsup <- data.registeredSuper
      ext <- allExtended(rsup.name)
      concrete <- { /*println(ext); */ data.concreteObjects(ext) }
    } yield {
      rsup.registerTo -> concrete.name
    }) ++ data.registries groupBy (_._1) mapValues (_.map(_._2).toSet)
    r*/
  }

  /*private def allExtended(from: String) = {
    @tailrec
    def loop(done: Seq[String], lst: String*): Seq[String] = {
      lst match {
        case Seq() =>
          done
        case head +: tail =>
          loop(done :+ head, ((data.extended(head) map (_.name)) ++ tail): _*)
      }
    }
    loop(Nil, from)
  }*/
}