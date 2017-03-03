package success.samefile.descendent

import autoregister.annotations._

object A {
  def register(toRegister: A): Unit = {}
}

@RegisterAllDescendentObjects("success.samefile.descendent.A.register")
trait A

object B extends A

class C {
  object D extends A
}