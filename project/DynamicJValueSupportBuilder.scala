import sbt._

object DynamicJValueSupportBuilder extends ((File, String) => Seq[File]) {
  def apply(root: File, scalaVersion: String): Seq[File] = {
    val outputDir = root / "com" / "rojoma" / "json-impl" / "dynamic"
    outputDir.mkdirs()
    val outputFile = outputDir / "DynamicJValueTypeShims.scala"
    val f = new java.io.OutputStreamWriter(new java.io.FileOutputStream(outputFile), "UTF-8")
    try {
      f.write("""package com.rojoma.`json-impl`
package dynamic
""")

      // In the future, if SIP-15 comes to pass, DynamicJValue may
      // inherit from Any instead.
      f.write("""
object BaseClassHolder {
  type BaseClass = AnyRef
}
""")

      if(scalaVersion startsWith "2.8.") {
        f.write("""
trait Dynamic
""")
      }
    } finally {
      f.close()
    }
    Seq(outputFile)
  }
}

