name := "rojoma-json-v3"

organization := "com.rojoma"

version := "3.13.1-SNAPSHOT"

mimaPreviousArtifacts := Set("com.rojoma" %% "rojoma-json-v3" % "3.13.0")

scalaVersion := "2.12.12"

crossScalaVersions := Seq("2.10.7", "2.11.12", scalaVersion.value)

scalacOptions ++= {
  val SV = """(\d+)\.(\d+)\..*""".r
  val optimizationOptions = scalaVersion.value match {
    case SV("2","10" | "11") =>
      List("-optimize")
    case SV("2","12") =>
      List("-opt:l:inline", "-opt-inline-from:com.rojoma.json.v3.**")
    case _ =>
      sys.error("Need to set up scalacoptions for the current compiler")
    }
  Seq("-deprecation", "-feature", "-Xlint") ++ optimizationOptions
}

Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oD")

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies ++= Seq(
  "com.rojoma" %% "rojoma-json" % "2.4.3" % "optional",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "optional" // optional because generators for JValues are included
)

libraryDependencies ++= {
  if(scalaVersion.value startsWith "2.10.")
    List("org.scalamacros" %% "quasiquotes" % "2.1.1")
  else
    Nil
}

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

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
