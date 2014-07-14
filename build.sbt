name := "rojoma-json-v3"

organization := "com.rojoma"

version := "3.0.0-SNAPSHOT"

scalaVersion := "2.11.1"

crossScalaVersions := Seq("2.10.4", scalaVersion.value)

scalacOptions ++= Seq("-deprecation", "-feature", "-optimize")

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD")

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies ++= Seq(
  "com.rojoma" %% "rojoma-json" % "2.4.3" % "optional",
  "org.scalatest" %% "scalatest" % "2.2.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.4" % "optional" // optional because generators for JValues are included
)

libraryDependencies ++= {
  if(scalaVersion.value startsWith "2.10.")
    List("org.scalamacros" %% "quasiquotes" % "2.0.0")
  else
    Nil
}

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0" cross CrossVersion.full)

sourceGenerators in Compile <+= (sourceManaged in Compile) map SimpleJsonCodecBuilderBuilder

sourceGenerators in Compile <+= (sourceManaged in Compile) map TupleCodecBuilder

unmanagedSourceDirectories in Compile += locally {
  val MajorMinor = """(\d+\.\d+)\..*""".r
  val dir = scalaVersion.value match {
    case MajorMinor(mm) => "scala-" + mm
    case _ => sys.error("Unable to find major/minor Scala version in " + scalaVersion)
  }
  (scalaSource in Compile).value.getParentFile / dir
}
