import sbt._

object ValueClassSupportBuilder extends ((File, String) => Seq[File]) {
  def apply(root: File, scalaVersion: String): Seq[File] =
    go(root, scalaVersion)(
      ("dynamic", "DynamicJValue", "static")
    )

  def go(root: File, scalaVersion: String)(classes: (String, String, String)*): Seq[File] =
    for {
      (packageName, className, fieldName) <- classes
    } yield emitShim(root, scalaVersion, packageName.split('.'), className, fieldName)

  def emitShim(root: File, scalaVersion: String, packagePath: Seq[String], className: String, field: String): File = {
    val outputDir = packagePath.foldLeft(root / "com" / "rojoma" / "json-impl")(_ / _)
    outputDir.mkdirs()
    val outputFile = outputDir / (className + "SuperClassHolder.scala")
    val f = new java.io.OutputStreamWriter(new java.io.FileOutputStream(outputFile), "UTF-8")
    try {
      f.write("package " + packagePath.foldLeft("com.rojoma.`json-impl`")(_ + "." + _) + "\n\n")

      // In the future, if SIP-15 comes to pass, SuperClass may become
      // a type alias for AnyVal.  equals and hashCode are automatically
      // provided and cannot be overriden on value classes as of this
      // writing, which is why they're here in the pre-SIP-15 shim.
      f.write("object " + className + "SuperClassHolder {\n")
      f.write("  import " + packagePath.foldLeft("com.rojoma.json")(_ + "." + _) + "." + className + "\n")
      f.write("  abstract class SuperClass { this: " + className + " =>\n")
      f.write("    override def hashCode = " + field+ ".hashCode\n")
      f.write("    override def equals(x: Any) = x match {\n")
      f.write("      case that: " + className + " => this." + field + " == that." + field + "\n")
      f.write("      case _ => false\n")
      f.write("    }\n")
      f.write("  }\n")
      f.write("}\n")
    } finally {
      f.close()
    }
    outputFile
  }
}

