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

      // In the future, if SIP-15 comes to pass, SuperClass may become
      // a type alias for AnyVal.  equals and hashCode are automatically
      // provided and cannot be overriden on value classes as of this
      // writing, which is why they're here in the pre-SIP-15 shim.
      f.write("""
object SuperClassHolder {
  import com.rojoma.json.dynamic.DynamicJValue
  abstract class SuperClass { this: DynamicJValue =>
    override def hashCode = static.hashCode
    override def equals(x: Any) = x match {
      case that: DynamicJValue => this.static == that.static
      case _ => false
    }
  }
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

