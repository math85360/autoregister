package success.withimplicits

import autoregister.annotations._

@RegisterAllDescendentObjects
trait NeedRegistering

object ToRegister extends NeedRegistering