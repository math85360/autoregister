package autoregister

import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.Settings
import java.net.URLClassLoader
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.Global
import scala.tools.nsc.plugins.Plugin
import utest._, asserts._
import scala.reflect.io.PlainDirectory
import scala.reflect.io.Directory
import scala.reflect.io.PlainFile
import scala.reflect.io.File
import java.io.FileInputStream

object TestUtils {
  def getFilePaths(src: String): List[String] = {
    val f = new java.io.File(src)
    if (f.isDirectory()) f.list().toList.flatMap(x => getFilePaths(s"$src/$x"))
    else List(src)
  }

  def make(path: String) = {
    val src = s"src/test/resources/$path"
    val sources = getFilePaths(src)

    val vd = new VirtualDirectory("(memory)", None)
    lazy val settings = new Settings
    val loader = getClass.getClassLoader.asInstanceOf[URLClassLoader]
    val entries = loader.getURLs map (_.getPath)
    settings.outputDirs.setSingleOutput(vd)

    val sclpath = entries.map(
      _.replaceAll("scala-compiler.jar", "scala-library.jar")
    )

    settings.classpath.value = ClassPath.join(entries ++ sclpath: _*)

    lazy val compiler = new Global(settings, new ConsoleReporter(settings)) {
      override protected def loadRoughPluginsList(): List[Plugin] = {
        List(new plugin.RuntimePlugin(this))
      }
    }

    val run = new compiler.Run()
    run.compile(sources)
    if (vd.toList.isEmpty) throw CompilationException()
  }

  case class CompilationException() extends Exception
}
