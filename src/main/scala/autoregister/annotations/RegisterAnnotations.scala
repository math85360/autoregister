package autoregister.annotations

/**
 * to: name of registry
 * if ignoreLackingRegistry is true, LackingRegistryException will not be
 * thrown
 */
class RegisterAllDescendentObjects(to: String, ignoreLackingRegistry: Boolean) extends scala.annotation.StaticAnnotation {
  def this() = this(null, false)
  def this(to: String) = this(to, false)
  def this(ignoreLackingRegistry: Boolean) = this(null, ignoreLackingRegistry)
}

/**
 *
 */
class Register(to: String, ignoreLackingRegistry: Boolean) extends scala.annotation.StaticAnnotation {
  def this(to: String) = this(to, false)
}