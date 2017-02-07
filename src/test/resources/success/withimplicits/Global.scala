package success.withimplicits

import autoregister.annotations._

object Registry {
  def register[T: Registerable](toRegister: T): Unit = {}

  register[NeedRegistering](ToRegister)
}
