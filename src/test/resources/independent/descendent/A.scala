package independent.descendent

import autoregister.annotations._

object A {
  def register(toRegister: A): Unit = {}
}

@RegisterAllDescendentObjects("independent.descendent.A.register")
trait A