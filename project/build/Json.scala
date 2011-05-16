import sbt._

class Json(info: ProjectInfo) extends DefaultProject(info) with rsync.RsyncPublishing {
  lazy val scalaTest = "org.scalatest" % "scalatest" % "1.3" % "test->default"
  lazy val scalaCheck = "org.scala-tools.testing" %% "scalacheck" % "1.8" % "test->default"

  def rsyncRepo = "rojoma.com:public_html/maven"
}
