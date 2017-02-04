package success.independent.descendent

import autoregister.annotations._

object A {
  def register(toRegister: A): Unit = {}
}

@RegisterAllDescendentObjects("success.independent.descendent.A.register")
trait A