package autoregister.annotations

/**
 */
class Registry(name: String) extends scala.annotation.StaticAnnotation {
  def this() = this(null)
}
