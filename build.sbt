organization := "com.rojoma"

name := "rojoma-json"

version := "2.0.0"

scalaVersion := "2.9.3"

crossScalaVersions := Seq("2.8.1", "2.8.2", "2.9.0", "2.9.0-1", "2.9.1", "2.9.1-1", "2.9.2", "2.9.3")

libraryDependencies <++= scalaVersion { sv =>
  sv match {
    case "2.8.1" | "2.8.2" => Seq(
      "org.scalacheck" % "scalacheck_2.8.1" % "1.8" % "optional",
      "org.scalatest" %% "scalatest" % "1.8" % "test"
    )
    case _ => Seq(
      "org.scalacheck" %% "scalacheck" % "1.10.1" % "optional",
      "org.scalatest" %% "scalatest" % "1.9.1" % "test"
    )
  }
}

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD")

// Include generated sources in source jar
mappings in (Compile, packageSrc) <++= (sourceManaged in Compile, managedSources in Compile) map { (base, srcs) =>
  import Path.{flat, relativeTo}
  srcs x (relativeTo(base) | flat)
}

sourceGenerators in Compile <+= (sourceManaged in Compile) map SimpleJsonCodecBuilderBuilder

sourceGenerators in Compile <+= (sourceManaged in Compile, scalaVersion in Compile) map DynamicJValueSupportBuilder

sourceGenerators in Compile <+= (sourceManaged in Compile, scalaVersion in Compile) map ValueClassSupportBuilder

// Bit of a hack; regenerate README.markdown when version is changed
// to a non-SNAPSHOT value.
sourceGenerators in Compile <+= (baseDirectory, version, crossScalaVersions) map READMEBuilder
