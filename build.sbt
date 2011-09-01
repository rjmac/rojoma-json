organization := "com.rojoma"

name := "rojoma-json"

version := "1.3.22"

crossScalaVersions := Seq("2.8.1", "2.9.0", "2.9.0-1", "2.9.1")

libraryDependencies <++= scalaVersion { sv =>
  sv match {
    case "2.8.1" => Seq(
      "org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" % "optional",
      "org.scalatest" % "scalatest_2.8.1" % "1.5.1" % "test"
    )
    case "2.9.0" | "2.9.0-1" => Seq(
      "org.scala-tools.testing" % "scalacheck_2.9.0" % "1.9" % "optional",
      "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test"
    )
    case _ => error("Dependencies not set for scala version " + sv)
  }
}
