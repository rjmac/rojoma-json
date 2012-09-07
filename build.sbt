import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys.previousArtifact

mimaDefaultSettings

organization := "com.rojoma"

name := "rojoma-json"

version := "2.0.1-SNAPSHOT"

previousArtifact <<= scalaVersion { sv => Some("com.rojoma" % ("rojoma-json_" + sv) % "2.0.0") }

scalaVersion := "2.9.2"

crossScalaVersions := Seq("2.8.1", "2.8.2", "2.9.0", "2.9.0-1", "2.9.1", "2.9.1-1", "2.9.2", "2.10.0-M7")

libraryDependencies <+= scalaVersion {
  case "2.10.0-M7" =>
    "org.scalatest" % "scalatest_2.10.0-M7" % "1.9-2.10.0-M7-B1"
  case _ =>
    "org.scalatest" %% "scalatest" % "1.8" % "test"
}

libraryDependencies <+= scalaVersion {
  case "2.8.1" | "2.8.2" =>
    "org.scalacheck" % "scalacheck_2.8.1" % "1.8" % "optional"
  case "2.10.0-M7" =>
    "org.scalacheck" % "scalacheck_2.10.0-M7" % "1.10.0"
  case _ =>
    "org.scalacheck" %% "scalacheck" % "1.9" % "optional"
}

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD")

scalacOptions <++= scalaVersion map { sv =>
  val base = Seq("-deprecation")
  val versionSpecific = if(sv.startsWith("2.10.")) Seq("-language:_")
                        else Nil
  base ++ versionSpecific
}

// Include generated sources in source jar
mappings in (Compile, packageSrc) <++= (sourceManaged in Compile, managedSources in Compile) map { (base, srcs) =>
  import Path.{flat, relativeTo}
  srcs x (relativeTo(base) | flat)
}

sourceGenerators in Compile <+= (sourceManaged in Compile) map SimpleJsonCodecBuilderBuilder

sourceGenerators in Compile <+= (sourceManaged in Compile, scalaVersion in Compile) map DynamicJValueSupportBuilder

sourceGenerators in Compile <+= (sourceManaged in Compile, scalaVersion in Compile) map ValueClassSupportBuilder

sourceGenerators in Compile <+= (sourceManaged in Compile, scalaVersion in Compile) map PackageObjectBuilder

sourceGenerators in Compile <+= (sourceManaged in Compile, scalaVersion in Compile) map JArrayShimBuilder

// Bit of a hack; regenerate README.markdown when version is changed
// to a non-SNAPSHOT value.
sourceGenerators in Compile <+= (baseDirectory, version, crossScalaVersions) map READMEBuilder
