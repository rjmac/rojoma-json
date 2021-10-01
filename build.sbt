name := "rojoma-json-v3"

organization := "com.rojoma"

version := "3.13.1-SNAPSHOT"

mimaPreviousArtifacts := Set("com.rojoma" %% "rojoma-json-v3" % "3.13.0")

scalaVersion := "2.13.5"

scalacOptions ++= {
  val SV = """(\d+)\.(\d+)\..*""".r
  val optimizationOptions = scalaVersion.value match {
    case SV("2","10" | "11") =>
      List("-optimize", "-Xlint")
    case SV("2","12") =>
      List("-opt:l:inline", "-opt-inline-from:com.rojoma.json.v3.**", "-Xlint")
    case SV("2","13") =>
      List("-opt:l:inline", "-opt-inline-from:com.rojoma.json.v3.**", "-Xlint", "-Xlint:-nonlocal-return", "-Xlog-free-types", "-Ymacro-annotations")
    case _ =>
      sys.error("Need to set up scalacoptions for the current compiler")
    }
  Seq("-deprecation", "-feature") ++ optimizationOptions
}

Compile / console / scalacOptions -= "-Xfatal-warnings"
Compile / console / scalacOptions += "-Xlint:-unused"

Compile / doc / scalacOptions -= "-Xfatal-warnings"

Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oD")

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "optional" // optional because generators for JValues are included
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
