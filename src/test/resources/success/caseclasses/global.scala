package success.caseclasses

import autoregister.annotations._
import scala.reflect.ClassTag

trait Registrable[T]

@RegisterAllDescendentCaseClasses
trait B

case class C(a: Any) extends B

object C {
  implicit object CRegistrable extends Registrable[C]
}

case class D() extends B

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
  case class Registered[R, U](cls: Class[R], app: (U) => R, unapp: (R) => U)(implicit ct: ClassTag[R])

  var registered = Map[ClassTag[_], Registered[_, _]]()

  @Registry
  def register[R: Registrable, U](cls: Class[R], app: (U) => R, unapp: (R) => U)(implicit ct: ClassTag[R]) {
    val x = implicitly[Registrable[R]]
    registered += ct -> Registered(cls, app, unapp)(ct)
  }

  def check[A, T](_v: A, _t: T)(implicit ct: ClassTag[A]) {
    val r = registered(ct).asInstanceOf[Registered[A, T]]
    val v = r.app(_t)
    assert(_v == v)
    val t = r.unapp(_v)
    assert(_t == t)
  }

  if (false) {
    register(classOf[C], (C.apply _), C.unapply)
    register(classOf[D], (_: Unit) => D.apply(), (_: D) => ())
    register(classOf[E], (E.apply _).tupled, (E.unapply _).andThen(_.get))
    register(classOf[G], (G.apply _).tupled, (G.unapply _).andThen(_.get))
    register(classOf[J.K], (J.K.apply _).tupled, (J.K.unapply _).andThen(_.get))
  }
}

class Test {
  val a = A.registered(ClassTag(classOf[C])).asInstanceOf[A.Registered[C, Any]]
  assert(a.app("test") == C("test"))
  assert(a.unapp(C(50.0)) == 50.0)
  val e = A.registered(ClassTag(classOf[E])).asInstanceOf[A.Registered[E, (String, Int, Double)]]
  assert(e.app("test", 5, 2.0) == E("test", 5, 2.0))
  assert(e.unapp(E("m", -2, -5.0)) == ("m", -2, -5.0))
  A.check(C(""), "")
  A.check(D(), (()))
  A.check(D(), ())
  A.check(E("", 1, 2.0), ("", 1, 2.0))
  A.check(J.K("a", 2, 4.0), ("a", 2, 4.0))
  assert(true)
}