import sbt._

object READMEBuilder extends ((File, String) => Seq[File]) {
  def apply(baseDirectory: File, version: String): Nil.type = {
    if(!version.endsWith("-SNAPSHOT")) {
      val in = scala.io.Source.fromFile(baseDirectory / "README.markdown.in")
      try {
        val out = new java.io.PrintWriter(baseDirectory / "README.markdown")
        try {
            for(line <- in.getLines()) {
              out.println(line.replaceAll(java.util.regex.Pattern.quote("%VERSION%"), java.util.regex.Matcher.quoteReplacement(version)))
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
}
