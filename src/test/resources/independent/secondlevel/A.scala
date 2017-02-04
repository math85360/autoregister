package independent.secondlevel

import autoregister.annotations._

object A {
  def register(toRegister: A): Unit = {}
}

@RegisterAllDescendentObjects("independent.secondlevel.A.register")
trait A