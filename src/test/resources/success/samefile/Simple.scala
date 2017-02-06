package success.samefile.simple

import autoregister.annotations._

@Registry
object A {
  var registered = Seq[String]()
  def register(any: AnyRef): Unit = registered :+= any.toString()
}

@Register("success.samefile.simple.A.register")
object B
