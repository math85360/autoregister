package success.samefile.simple

import autoregister.annotations._

object A {
  @Registry
  def register(any: AnyRef): Unit = {}
}

@Register("success.samefile.simple.A.register")
object B
