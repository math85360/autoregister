package success.withimplicits

sealed trait Registerable[+T]

object Registerable {
  implicit object ToRegisterable extends Registerable[NeedRegistering]
}
