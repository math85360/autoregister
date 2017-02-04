# autoregister 0.0.2 [![Build Status][travis-badge]][travis-link]
===============

[travis-badge]: https://travis-ci.org/math85360/autoregister.svg
[travis-link]: https://travis-ci.org/math85360/autoregister

**autoregister** is a Scala compiler plugin that finds / discovers all classes having a specific annotations and push them in a registry.

```scala
package myframework

import autoregister.annotations._

@RegisterAllDescendentObjects(to = "ControllerRegistry.register")
trait Controller

object ControllerRegistry {
	var registered = Seq[Controller]()

	// Keep it fastest as possible
	def register(controller: Controller): Unit = {
		registered += controller
	}
}
```

```scala
package mymodule

import myframework._

object MyController extends Controller {
	// Don't make slow initialization or use lazy val instead of val
	// Because all registered components will be initialized 
	// at first use of ControllerRegistry
}
```

This scala plugin will transform ControllerRegistry like this :

```scala
package myframework

import autoregister.annotations._

@RegisterAllDescendentObjects(to = "ControllerRegistry.register")
trait Controller

object ControllerRegistry {
	var registered = Seq[Controller]()

	// Keep it fastest as possible
	def register(controller: Controller): Unit = {
		registered += controller
	}

	register(mymodule.MyController)
}
```

## ScalaJS usage to register to raw JavaScript objects
===================================================

```scala
@JSName("Registry")
object Registry extends js.Object {
	def register(toRegister: ToRegister): Unit = js.native
}

object RegistryProxy {
	def register(toRegister: ToRegister): Unit = Registry.register(toRegister)

	// We need this proxy because registered object will be added to end of this object like this :
	// register(SimpleController)
}

@ExportAllDescendentObjects(to = "RegistryProxy.register")
trait ToRegister extends js.Object

trait Controller extends ToRegister

object SimpleController extends Controller
```

## Some typical usage cases
========================

### Angular in scalaJS

You can transform this :

```scala
object Main {
	def main() {
		Angular.module("MyModule")
			.controller(MyController)
			.service(MyService)
	}
}

object MyController extends Controller

object MyService extends Service
```

into

```scala
object Main {
	def main() {
		Registry.toRegisterByAngular.foldLeft(Angular.module("MyModule")) {
			(module, registerable) => registerable(module)
		} 
	}
}

trait Registerable[T] {
	def register(module: Module, t: T): Module
}

object Registerable {
	implicit object ControllerRegisterable extends Registerable[Controller] {
		override def register(module: Module, t: Controller) = module.controller(t)
	}
	implicit object ControllerRegisterable extends Registerable[Service] {
		override def register(module: Module, t: Service) = module.service(t)
	}
}

object Registry {
	var toRegisterByAngular = Seq[Module => Module]()

	def register[T:Registerable](toRegister: T): Unit = {
		toRegisterByAngular += implicitly[Registerable[T]].register(_, toRegister)
	}
}

@RegisterAllDescendentObjects(to = "Registry.register")
trait MyModuleRegister

object MyController extends Controller with MyModuleRegister

object MyService extends Service with MyModuleRegister
```

When you add a new controller or service, you don't need anymore to add a line in the main part (just "with MyModuleRegister" to your object). Great ! 

## How to Use
==========

**Need testing before publishing, please wait**

**If you need it now, clone this project in a folder and do ```sbt publish-local```**

To use, add the following to your `build.sbt`:

```scala
libraryDependencies += "com.iz2use" %% "autoregister" % "0.0.2" % "provided"

autoCompilerPlugins := true

addCompilerPlugin("com.iz2use" %% "autoregister" % "0.0.2")
```

## Limitations
===========

For the first version, all registry and objects to register must be defined in the same build.

You can not have a library defining annotations with Registry and use them in your project importing this library.

To bypass this limit, you can use the @Registry annotation on a method defined on your own project :

```scala
import autoregister.annotations._

object Main {
	@Registry
	def register(any: AnyRef) : Unit {
	}

	def main() {
	}
}
```

## ChangeLog
=========

## MIT License
===========

The MIT License (MIT)

Copyright (c) 2017 Mathieu Leguey

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.