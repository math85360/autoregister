package success.independent.secondlevel

import autoregister.annotations._

object A {
  def register(toRegister: A): Unit = {}
}

@RegisterAllDescendentObjects("success.independent.secondlevel.A.register")
trait A