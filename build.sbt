import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys.previousArtifact

mimaDefaultSettings

net.virtualvoid.sbt.graph.Plugin.graphSettings

organization := "com.rojoma"

name := "rojoma-json"

version := "2.4.0-SNAPSHOT"

previousArtifact <<= scalaBinaryVersion { sv => Some("com.rojoma" % ("rojoma-json_" + sv) % "2.3.0") }

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.10.0" % "optional"
)

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD")

scalacOptions ++= Seq("-deprecation","-feature")

// Include generated sources in source jar
mappings in (Compile, packageSrc) <++= (sourceManaged in Compile, managedSources in Compile) map { (base, srcs) =>
  import Path.{flat, relativeTo}
  srcs x (relativeTo(base) | flat)
}

sourceGenerators in Compile <+= (sourceManaged in Compile) map SimpleJsonCodecBuilderBuilder

sourceGenerators in Compile <+= (sourceManaged in Compile) map TupleCodecBuilder

// Bit of a hack; regenerate README.markdown when version is changed
// to a non-SNAPSHOT value.
sourceGenerators in Compile <+= (baseDirectory, version, crossScalaVersions) map READMEBuilder
