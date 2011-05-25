import sbt._

class Json(info: ProjectInfo) extends DefaultProject(info) with rsync.RsyncPublishing {
  lazy val scalaCheck = "org.scala-tools.testing" %% "scalacheck" % "1.8" % "optional"
  val scalaTest =
    buildScalaVersion match {
      case "2.8.1" => "org.scalatest" % "scalatest" % "1.3"
      case "2.9.0" => "org.scalatest" %% "scalatest" % "1.4.1"
      case x => error("Unsupported Scala version " + x)
    }

  override def packageSrcJar = defaultJarPath("-sources.jar")
  val sourceArtifact = Artifact.sources(artifactID)

  override def artifacts = super.artifacts ++ Seq(sourceArtifact)

  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageSrc)

  def rsyncRepo = "rojoma.com:public_html/maven"
}
