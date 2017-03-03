package success.classes

import autoregister.annotations._

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
  @Registry
  def register[R: Registrable](cls: Class[R]) {
    val x = implicitly[Registrable[R]]
  }

  register(classOf[C])
  register(classOf[D])
  register(classOf[E])
  register(classOf[F])
}