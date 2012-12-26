import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys.previousArtifact

mimaDefaultSettings

organization := "com.rojoma"

name := "rojoma-json"

version := "2.1.1-SNAPSHOT"

previousArtifact <<= scalaBinaryVersion { sv => Some("com.rojoma" % ("rojoma-json_" + sv) % "2.1.0") }

scalaVersion := "2.10.0"

// test-libraries not built for 2.11 yet, of course...
// libraryDependencies ++= Seq(
//   "org.scalatest" %% "scalatest" % "1.9.1" % "test",
//   "org.scalacheck" %% "scalacheck" % "1.10.0" % "optional"
// )

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD")

scalacOptions ++= Seq("-deprecation", "-language:_")

// Include generated sources in source jar
mappings in (Compile, packageSrc) <++= (sourceManaged in Compile, managedSources in Compile) map { (base, srcs) =>
  import Path.{flat, relativeTo}
  srcs x (relativeTo(base) | flat)
}

sourceGenerators in Compile <+= (sourceManaged in Compile) map SimpleJsonCodecBuilderBuilder

// Bit of a hack; regenerate README.markdown when version is changed
// to a non-SNAPSHOT value.
sourceGenerators in Compile <+= (baseDirectory, version, crossScalaVersions) map READMEBuilder

// macro-paradise!

scalaVersion := "2.11.0-SNAPSHOT"

// FIXME: remove once these are published for 2.11...
libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
  "org.scalacheck" % "scalacheck_2.10" % "1.10.0" % "optional"
)

scalaOrganization := "org.scala-lang.macro-paradise"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies <+= (scalaVersion)("org.scala-lang.macro-paradise" % "scala-reflect" % _)
