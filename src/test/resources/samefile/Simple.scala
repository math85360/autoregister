package samefile.simple

import autoregister.annotations._

@Registry
object A {
  def register(toRegister: AnyRef): Unit = {}
}

@Register("samefile.simple.A.register")
object B