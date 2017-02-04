package samefile.secondlevel

import autoregister.annotations._

object A {
  def register(toRegister: A): Unit = {}
}

@RegisterAllDescendentObjects("samefile.secondlevel.A.register")
trait A

class B extends A

object C extends B