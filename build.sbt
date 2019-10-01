
organization  := "com.iz2use"

name := "autoregister"

version := "0.0.6"

//scalaVersion  := "2.11.8"

crossScalaVersions := Seq("2.11.8", "2.12.0")

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "utest" % "0.4.4" % "test",
  "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
)

testFrameworks += new TestFramework("utest.runner.Framework")

// Needed for eclipse or other to see code as scala source but is in conflict for tests
// unmanagedSourceDirectories in Test <+= baseDirectory(_ / "src" / "test" / "resources")

// Sonatype
publishArtifact in Test := false

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomExtra := (
  <url>https://github.com/math85360/autoregister</url>
    <licenses>
      <license>
        <name>MIT license</name>
        <url>http://www.opensource.org/licenses/mit-license.php</url>
      </license>
    </licenses>
    <scm>
      <url>git://github.com/math85360/autoregister</url>
      <connection>scm:git://github.com/math85360/autoregister.git</connection>
    </scm>
    <developers>
      <developer>
        <id>math85360</id>
        <name>Mathieu Leguey</name>
        <url>https://github.com/math85360</url>
      </developer>
    </developers>
  )