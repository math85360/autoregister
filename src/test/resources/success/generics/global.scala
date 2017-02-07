package success.generics

import autoregister.annotations._

@RegisterAllDescendentObjects
trait A[U, V]

class B[W <: String, X, Y, Z] extends A[W, X]

object C extends B[String, Int, Double, AnyRef]

trait Registerable[T]

trait LowPriority {
  implicit def CIsRegisterable[T <: A[_, _]] = new Registerable[T] {
  }
}

object Registerable extends LowPriority {

}

object A {
  @Registry
  def register[A: Registerable, T <: A](toReg: T): Unit = {}

  register(C)
}
