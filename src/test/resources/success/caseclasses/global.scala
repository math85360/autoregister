package success.caseclasses

import autoregister.annotations._
import scala.reflect.ClassTag

trait Registrable[T]

@RegisterAllDescendentCaseClasses
trait B

case class C(a: Any, b: Double) extends B

object C {
  implicit object CRegistrable extends Registrable[C]
}

case class D(a: Any, b: Double) extends B

object D {
  implicit object DRegistrable extends Registrable[D]
}

case class E(a: String, b: Int, c: Double) extends B

object E {
  implicit object ERegistrable extends Registrable[E]
}

abstract class F extends B

case class G(a: String, b: Int, c: Double) extends F

object G {
  implicit object GRegistrable extends Registrable[G]
}

class H {
  case class I(a: String, b: Int, c: Double) extends B
}

object J {
  case class K(a: String, b: Int, c: Double) extends B
  object K {
    implicit object KRegistrable extends Registrable[K]
  }
}

object A {
  @Registry
  def register[R: Registrable, U](cls: Class[R], app: (U) => R, unapp: (R) => Option[U])(implicit ct: ClassTag[R]) {
    val x = implicitly[Registrable[R]]
    //cls.newInstance()
  }

  register(classOf[C], (C.apply _).tupled, C.unapply)
  register(classOf[D], (D.apply _).tupled, D.unapply)
  register(classOf[E], (E.apply _).tupled, E.unapply)
  register(classOf[G], (G.apply _).tupled, G.unapply)
  register(classOf[J.K], (J.K.apply _).tupled, J.K.unapply)

}