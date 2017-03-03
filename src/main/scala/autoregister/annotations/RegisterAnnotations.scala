package autoregister.annotations

/**
 * @to: name of register method of registry
 * Signature of register method must have only one argument
 * Method can have implicit evidence
 */
class RegisterAllDescendentObjects(to: String) extends scala.annotation.StaticAnnotation {
  def this() = this(null)
}

/**
 * @to: name of register method of registry
 * Signature of register method must be [A](Class[A]): Unit
 * "A" could be A <: Super or A:Registerable
 * Method can have implicit evidence
 */
class RegisterAllDescendentConcreteClasses(to: String) extends scala.annotation.StaticAnnotation {
  def this() = this(null)
}

/**
 * @to: name of register method of registry
 * Signature of register method must be [A, B](Class[A], A => B, B => A): Unit
 * Method can have implicit evidence
 */
class RegisterAllDescendentCaseClasses(to: String) extends scala.annotation.StaticAnnotation {
  def this() = this(null)
}

/**
 *
 */
class Register(as: String) extends scala.annotation.StaticAnnotation {
}