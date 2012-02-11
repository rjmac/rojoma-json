organization := "com.rojoma"

name := "rojoma-json"

version := "1.4.2-SNAPSHOT"

crossScalaVersions := Seq("2.8.1", "2.8.2", "2.9.0", "2.9.0-1", "2.9.1")

libraryDependencies <++= scalaVersion { sv =>
  sv match {
    case "2.8.1" | "2.8.2" => Seq(
      "org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" % "optional",
      "org.scalatest" % "scalatest_2.8.1" % "1.5.1" % "test"
    )
    case "2.9.0" | "2.9.0-1" | "2.9.1" => Seq(
      "org.scala-tools.testing" % "scalacheck_2.9.0" % "1.9" % "optional",
      "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test"
    )
    case _ => error("Dependencies not set for scala version " + sv)
  }
}

// Include generated sources in source jar
mappings in (Compile, packageSrc) <++= (sourceManaged in Compile, managedSources in Compile) map { (base, srcs) =>
  import Path.{flat, relativeTo}
  srcs x (relativeTo(base) | flat)
}

sourceGenerators in Compile <+= (sourceManaged in Compile) map SimpleJsonCodecBuilderBuilder

sourceGenerators in Compile <+= (sourceManaged in Compile, scalaVersion in Compile) map DynamicJValueSupportBuilder

// Bit of a hack; regenerate README.markdown when version is changed
// to a non-SNAPSHOT value.
sourceGenerators in Compile <+= (baseDirectory, version) map READMEBuilder
