import sbt._

object JArrayShimBuilder extends ((File, String) => Seq[File]) {
  def apply(root: File, scalaVersion: String): Seq[File] = {
    val outputDir = root / "com" / "rojoma" / "json-impl" / "ast"
    outputDir.mkdirs()
    val outputFile = outputDir / "package.scala"
    val f = new java.io.OutputStreamWriter(new java.io.FileOutputStream(outputFile), "UTF-8")
    try {
      f.write("package com.rojoma.`json-impl`.ast\n\n")
      f.write("\n")
      f.write("import com.rojoma.json.ast._\n")
      f.write("\n")

      f.write("  trait JArrayShim extends Iterable[JValue] {\n")
      f.write("    def elems: Seq[JValue]\n")
      if(scalaVersion.startsWith("2.8.") || scalaVersion.startsWith("2.9.")) {
        f.write("  override def toIndexedSeq[B >: JValue] = elems.toIndexedSeq[B]\n")
      } else {
        f.write("  override def toIndexedSeq = elems.toIndexedSeq\n")
      }
      f.write("}\n")
    } finally {
      f.close()
    }

    Seq(outputFile)
  }
}
