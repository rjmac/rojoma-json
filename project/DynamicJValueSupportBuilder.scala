import sbt._

object DynamicJValueSupportBuilder extends ((File, String) => Seq[File]) {
  def apply(root: File, scalaVersion: String): Seq[File] = {
    if(scalaVersion startsWith "2.8.") {
      val outputDir = root / "com" / "rojoma" / "json-impl" / "dynamic"
      outputDir.mkdirs()
      val outputFile = outputDir / "Dynamic.scala"
      val f = new java.io.OutputStreamWriter(new java.io.FileOutputStream(outputFile), "UTF-8")
      try {
        f.write("""
package com.rojoma.`json-impl`.dynamic

trait Dynamic
""")
      } finally {
        f.close()
      }

      List(outputFile)
    } else {
      Nil
    }
  }
}

