package samefile.descendent

import autoregister.annotations._

object A {
  def register(toRegister: A): Unit = {}
}

@RegisterAllDescendentObjects("samefile.descendent.A.register")
trait A

object B extends A