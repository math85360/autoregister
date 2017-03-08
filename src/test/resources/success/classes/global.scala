package success.classes

import autoregister.annotations._
import scala.reflect.ClassTag

trait Registrable[T]

@RegisterAllDescendentConcreteClasses
trait B

final class C extends B

object C {
  implicit object CRegistrable extends Registrable[C]
}

case class D() extends B

object D {
  implicit object DRegistrable extends Registrable[D]
}

abstract class E extends B

object E {
  implicit object ERegistrable extends Registrable[E]
}

class F extends E

object F {
  implicit object FRegistrable extends Registrable[F]
}

class G {
  class H extends B
}

object A {
  var registered = Map[ClassTag[_], Class[_]]()

  @Registry
  def register[R: Registrable](cls: Class[R])(implicit ct: ClassTag[R]) {
    val x = implicitly[Registrable[R]]
    registered += ct -> cls
  }

  def check[A](_v: A)(implicit ct: ClassTag[A]) {
    val r = registered(ct).asInstanceOf[Class[A]]
    assert(r != null)
  }

  if (false) {
    register(classOf[C])
    register(classOf[D])
    register(classOf[E])
    register(classOf[F])
  }
}

class Test {
  A.check(new C())
  A.check(D())
  A.check(new F())
}