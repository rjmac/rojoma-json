import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys.previousArtifact

mimaDefaultSettings

net.virtualvoid.sbt.graph.Plugin.graphSettings

organization := "com.rojoma"

name := "rojoma-json"

version := "2.4.3-SNAPSHOT"

previousArtifact <<= scalaBinaryVersion { sv => Some("com.rojoma" % ("rojoma-json_" + sv) % "2.4.1") }

scalaVersion := "2.10.4"

crossScalaVersions := Seq("2.10.4", "2.11.0")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.1.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "optional"
)

libraryDependencies <++= (scalaVersion) {
  case sv if sv.startsWith("2.10.") =>
    List("org.scalamacros" %% "quasiquotes" % "2.0.0")
  case sv =>
    List("org.scala-lang.modules" %% "scala-xml" % "1.0.1")
}

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


resolvers += Resolver.sonatypeRepo("snapshots")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0" cross CrossVersion.full)
