package success.globalregistry

import autoregister.annotations._

object A {
  @Registry
  def register(any: AnyRef): Unit = {}
}

@RegisterAllDescendentObjects
trait A

class B extends A

object C extends B