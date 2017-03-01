package autoregister.annotations

/**
 * to: name of registry
 */
class RegisterAllDescendentObjects(to: String) extends scala.annotation.StaticAnnotation {
  def this() = this(null)
}

/**
 * to: name of registry
 */
class RegisterAllDescendentConcreteClasses(to: String) extends scala.annotation.StaticAnnotation {
  def this() = this(null)
}

/**
 *
 */
class Register(to: String) extends scala.annotation.StaticAnnotation {
}