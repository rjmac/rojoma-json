import sbt._

class Json(info: ProjectInfo) extends DefaultProject(info) {
  override def compileOptions =
    compileOptions("-encoding", "UTF-8") ++ compileOptions("-g") ++ compileOptions("-unchecked") ++ super.compileOptions

  lazy val scalaTest = "org.scalatest" % "scalatest" % "1.3" % "test->default"
  lazy val scalaCheck = "org.scala-tools.testing" %% "scalacheck" % "1.8" % "test->default"
}
