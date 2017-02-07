package success.withimplicits

import autoregister.annotations._

@RegisterAllDescendentObjects
trait NeedRegistering

object ToRegister extends NeedRegistering

sealed trait Registerable[T]

object Registerable {
  //implicit object ToRegisterable extends Registerable[NeedRegistering]
  implicit def toRegisterable[T] = new Registerable[T] {}
}

object Registry {
  def register[T: Registerable](toRegister: T): Unit = {}

  register(ToRegister)
}
