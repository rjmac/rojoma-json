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

      // In the future, if SIP-15 comes to pass, SuperClass may become
      // a type alias for AnyVal.  equals and hashCode are automatically
      // provided and cannot be overriden on value classes as of this
      // writing, which is why they're here in the pre-SIP-15 shim.
      f.write("  trait JArrayShim extends Iterable[JValue] {\n")
      f.write("    def elems: Seq[JValue]\n")
      if(scalaVersion.startsWith("2.10.")) {
        f.write("  override def toIndexedSeq = elems.toIndexedSeq\n")
      } else {
        f.write("  override def toIndexedSeq[B >: JValue] = elems.toIndexedSeq[B]\n")
      }
      f.write("}\n")
    } finally {
      f.close()
    }

    Seq(outputFile)
  }
}