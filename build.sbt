name := "rojoma-json-v3"

organization := "com.rojoma"

version := "3.12.1-SNAPSHOT"

mimaPreviousArtifacts := Set()//"com.rojoma" %% "rojoma-json-v3" % "3.12.0")

scalaVersion := "3.0.0"

scalacOptions ++= {
  val SV = """(\d+)\.(\d+)\..*""".r
  val optimizationOptions = scalaVersion.value match {
    case SV("3","0") =>
      List()
    case _ =>
      sys.error("Need to set up scalacoptions for the current compiler")
    }
  Seq("-deprecation", "-feature") ++ optimizationOptions
}

Compile / console / scalacOptions -= "-Xfatal-warnings"
Compile / console / scalacOptions += "-Xlint:-unused"

Compile / doc / scalacOptions -= "-Xfatal-warnings"

Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.9" % "test",
  "org.scalatestplus" %% "scalacheck-1-15" % "3.2.9.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.15.4" % "optional" // optional because generators for JValues are included
)

Compile / sourceGenerators += Def.task { SimpleJsonCodecBuilderBuilder((Compile / sourceManaged).value) }

Compile / sourceGenerators += Def.task { TupleCodecBuilder((Compile / sourceManaged).value) }

// Bit of a hack; regenerate README.markdown when version is changed
// to a non-SNAPSHOT value.
Compile / sourceGenerators += Def.task { READMEBuilder(baseDirectory.value, version.value, crossScalaVersions.value) }

Compile / unmanagedSourceDirectories += locally {
  val MajorMinor = """(\d+\.\d+)\..*""".r
  val dir = scalaVersion.value match {
    case MajorMinor(mm) => "scala-" + mm
    case _ => sys.error("Unable to find major/minor Scala version in " + scalaVersion)
  }
  (Compile / scalaSource).value.getParentFile / dir
}

// Include generated sources in source jar
Compile / packageSrc / mappings ++= {
  val base = (Compile / sourceManaged).value
  val srcs = (Compile / managedSources).value
  import Path.{flat, relativeTo}
  srcs pair (relativeTo(base) | flat)
}
