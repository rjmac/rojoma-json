import sbt._

object PackageObjectBuilder extends ((File, String) => Seq[File]) {
  def apply(root: File, scalaVersion: String): Seq[File] = {
    val outputDir = root / "com" / "rojoma" / "json-impl"
    outputDir.mkdirs()
    val outputFile = outputDir / "package.scala"
    val f = new java.io.OutputStreamWriter(new java.io.FileOutputStream(outputFile), "UTF-8")
    try {
      f.write("package com.rojoma\n\n")

      f.write("/** This package and its sub-packages contain things which for\n")
      f.write(" * various reasons need to be public but which should never be\n")
      f.write(" * explicitly used by user code.  Binary compatibility guarantees\n")
      f.write(" * hold only so long as this is true. */\n")
      f.write("package object `json-impl` {\n")
      if(scalaVersion.startsWith("2.8.") || scalaVersion.startsWith("2.9.")) {
        f.write("  type CM[A] = scala.reflect.ClassManifest[A]\n")
        f.write("  type M[A] = scala.reflect.Manifest[A]\n")
        f.write("  def erasureOf[A](cm: CM[A]) = cm.erasure\n")
      } else {
        f.write("  type CM[A] = scala.reflect.ClassTag[A]\n")
        f.write("  type M[A] = scala.reflect.ClassTag[A]\n")
        f.write("  def erasureOf[A](cm: CM[A]) = cm.runtimeClass\n")
      }

      f.write("}\n")
    } finally {
      f.close()
    }

    Seq(outputFile)
  }
}

