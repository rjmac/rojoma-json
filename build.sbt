name := "rojoma-json-v3"

organization := "com.rojoma"

version := "3.11.0-SNAPSHOT"

mimaPreviousArtifacts := Set("com.rojoma" % ("rojoma-json-v3_" + scalaBinaryVersion.value) % "3.10.1")

scalaVersion := "2.12.0"

crossScalaVersions := Seq("2.10.6", "2.11.8", scalaVersion.value)

scalacOptions ++= {
  val SV = """(\d+)\.(\d+)\..*""".r
  val optimizationOptions = scalaVersion.value match {
    case SV("2","10" | "11") =>
      List("-optimize")
    case SV("2","12") =>
      List("-opt:l:classpath")
    case _ =>
      sys.error("Need to set up scalacoptions for the current compiler")
    }
  Seq("-deprecation", "-feature", "-Xlint") ++ optimizationOptions
}

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD")

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies ++= Seq(
  "com.rojoma" %% "rojoma-json" % "2.4.3" % "optional",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "optional" // optional because generators for JValues are included
)

libraryDependencies ++= {
  if(scalaVersion.value startsWith "2.10.")
    List("org.scalamacros" %% "quasiquotes" % "2.1.0")
  else
    Nil
}

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

sourceGenerators in Compile += Def.task { SimpleJsonCodecBuilderBuilder((sourceManaged in Compile).value) }

sourceGenerators in Compile += Def.task { TupleCodecBuilder((sourceManaged in Compile).value) }

// Bit of a hack; regenerate README.markdown when version is changed
// to a non-SNAPSHOT value.
sourceGenerators in Compile += Def.task { READMEBuilder(baseDirectory.value, version.value, crossScalaVersions.value) }

unmanagedSourceDirectories in Compile += locally {
  val MajorMinor = """(\d+\.\d+)\..*""".r
  val dir = scalaVersion.value match {
    case MajorMinor(mm) => "scala-" + mm
    case _ => sys.error("Unable to find major/minor Scala version in " + scalaVersion)
  }
  (scalaSource in Compile).value.getParentFile / dir
}

// Include generated sources in source jar
mappings in (Compile, packageSrc) ++= {
  val base = (sourceManaged in Compile).value
  val srcs = (managedSources in Compile).value
  import Path.{flat, relativeTo}
  srcs pair (relativeTo(base) | flat)
}
