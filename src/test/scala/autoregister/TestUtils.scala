package autoregister

import java.lang.reflect.InvocationTargetException
import java.net.URLClassLoader

import scala.reflect.internal.util.{ AbstractFileClassLoader, BatchSourceFile }
import scala.reflect.io.{ AbstractFile, VirtualDirectory }
import scala.tools.nsc.{ Global, Settings }
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.util.ClassPath

object TestUtils {
  def getFilePaths(src: String): List[String] = {
    val f = new java.io.File(src)
    if (f.isDirectory()) f.list().toList.flatMap(x => getFilePaths(s"$src/$x"))
    else List(src)
  }

  def make(path: String)(value: Map[Option[String], Set[String]]) = {
    val src = s"src/test/resources/$path"

    val target = new VirtualDirectory("(memory)", None)
    lazy val settings = new Settings
    val loader = getClass.getClassLoader.asInstanceOf[URLClassLoader]
    val entries = loader.getURLs map (_.getPath)
    settings.outputDirs.add(AbstractFile.getDirectory("src/test/resources"), target)

    val sclpath = entries.map(
      _.replaceAll("scala-compiler.jar", "scala-library.jar")
    )

    settings.classpath.value = ClassPath.join(entries ++ sclpath: _*)

    var objectRegistered = Map[Option[String], Set[String]]()

    def report(key: Option[String], concretes: Set[String]) {
      if (concretes.nonEmpty)
        objectRegistered += key -> concretes
    }

    lazy val compiler = new Global(settings, new ConsoleReporter(settings)) {
      override protected def loadRoughPluginsList(): List[Plugin] = {
        List(new plugin.TestPlugin(this, plugin.Registry(), report))
      }
    }

    val run = new compiler.Run()
    val sources = getFilePaths(src)
    run.compileSources(sources.map(s => new BatchSourceFile(AbstractFile.getFile(s))))
    val classLoader = new AbstractFileClassLoader(target, this.getClass.getClassLoader)
    try {
      val name = path.replace('/', '.').concat("Test")
      val cls = classLoader.loadClass(name)
      val inst = cls.getConstructor().newInstance().asInstanceOf[Any]
    }
    catch {
      case e: ClassNotFoundException    =>
      case e: InvocationTargetException => throw e.getCause
    }
    if (target.toList.isEmpty) throw CompilationException()

    assert(value.toSeq.sortBy(_._1) == objectRegistered.toSeq.sortBy(_._1))
  }

  case class CompilationException() extends Exception
}
