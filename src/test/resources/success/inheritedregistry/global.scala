package success.inheritedregistry

import autoregister.annotations._

class B {
  @Registry
  def register(any: AnyRef): Unit = {}
}

object A extends B

@Register("success.inheritedregistry.A.register")
object B
