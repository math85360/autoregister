package success.samefile.simple

import autoregister.annotations._

@Registry
object A {
  var registered = Seq[String]()
  def register(any: AnyRef): Unit = registered :+= any.toString()
  register(B)
}

@Register("success.samefile.simple.A.register")
object B

/*@Register("success.samefile.simple.A.register")
object C*/

/*object C {
  var registered = Seq[String]()
  def register(any: AnyRef): Unit = registered :+= any.toString()
  register(success.samefile.simple.B)
}*/ 