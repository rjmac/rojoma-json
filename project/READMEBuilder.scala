import sbt._

object READMEBuilder extends ((File, String, Seq[String]) => Seq[File]) {
  def apply(baseDirectory: File, version: String, scalaVersions: Seq[String]): Nil.type = {
    if(!version.endsWith("-SNAPSHOT")) {
      val scalaVersionList = englishList(scalaVersions)
      val in = scala.io.Source.fromFile(baseDirectory / "README.markdown.in")
      try {
        val out = new java.io.PrintWriter(baseDirectory / "README.markdown")
        try {
            for(line <- in.getLines()) {
              out.println(xform(line, "%VERSION%" -> version, "%SCALAVERSIONS%" -> scalaVersionList))
            }
        } finally {
          out.close()
        }
      } finally {
        in.close()
      }
    }
    Nil
  }

  def englishList(xs: Seq[String]): String = xs.length match {
    case 0 => sys.error("empty list")
    case 1 => xs.head
    case 2 => xs(0) + " and " + xs(1)
    case _ => xs.dropRight(1).mkString(", ") + ", and " + xs.last
  }

  def xform(line: String, replacements: (String, String)*): String = {
    replacements.foldLeft(line) { (xformed, kv) =>
      val (k,v) = kv
      xformed.replaceAll(java.util.regex.Pattern.quote(k), java.util.regex.Matcher.quoteReplacement(v))
    }
  }
}
