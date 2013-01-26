import sbt._

object TupleCodecBuilder extends (File => Seq[File]) {
  def apply(root: File): Seq[File] = {
    val outputDir = root / "com" / "rojoma" / "json-impl" / "codec"
    outputDir.mkdirs()

    val outputFile = outputDir / ("TupleCodecs.scala")
    val f = new java.io.OutputStreamWriter(new java.io.FileOutputStream(outputFile), "UTF-8")

    try {
      f.write("""package com.rojoma.`json-impl`
package codec

import com.rojoma.json.codec._
import com.rojoma.json.ast.{JValue, JArray}

class TupleCodecs {
""")
      for(i <- 2 to 22) {
        f.write(genTupleCodec(i))
      }
      f.write("}\n")
    } finally {
      f.close()
    }
    Seq(outputFile)
  }

  def genTupleCodec(n: Int): String = {
    val sb = new StringBuilder

    def nameBase(i: Int) = ('A' + i - 1).toChar
    def typeName(i: Int) = nameBase(i).toString
    def subcodecName(i: Int) = "c" + nameBase(i)
    def elemName(i: Int) = "e" + nameBase(i)
    def decodedName(i: Int) = "d" + nameBase(i)

    val typeNames = (1 to n).map(typeName)
    val elemNames = (1 to n).map(elemName)
    val decodedNames = (1 to n).map(decodedName)
    def mapNames1[T](a: Int => String)(f: String => T) = (1 to n).map(a).map(f)
    def mapNamesIdx[T](a: Int => String)(f: (Int, String) => T) = (1 to n).map { i => f(i, a(i)) }
    def mapNames2[T](a: Int => String, b: Int => String)(f: (String, String) => T) = (1 to n).map(x => (a(x), b(x))).map(f.tupled)
    def mapNames3[T](a: Int => String, b: Int => String, c: Int => String)(f: (String, String, String) => T) = (1 to n).map(x => (a(x), b(x), c(x))).map(f.tupled)

    sb.append("  implicit def tuple").append(n).append("Codec").append(typeNames.mkString("[",",","]")).append("( implicit ").append(mapNames2(subcodecName, typeName) { (sc,t) => sc + ": JsonCodec[" + t + "]" }.mkString(",")).append(") = new JsonCodec[").append(typeNames.mkString("(",",",")")).append("] {\n")
    sb.append("    def encode(tuple: ").append(typeNames.mkString("(",",",")")).append("): JArray = {\n")
    sb.append("      JArray(Seq(").append(mapNamesIdx(subcodecName) { (i, c) => c + ".encode(tuple._" + i + ")" }.mkString(",")).append("))\n")
    sb.append("    }\n")
    sb.append("    def decode(jvalue: JValue): Option[(").append(typeNames.mkString(",")).append(")] = jvalue match {\n")
    sb.append("      case JArray(Seq(").append(elemNames.mkString(",")).append(")) =>\n")
    for(line <- mapNames3(decodedName, subcodecName, elemName)("        val " + _ + " = " + _ + ".decode(" + _ + ") match { case Some(result) => result; case _ => return None }\n")) {
      sb.append(line)
    }
    sb.append("        Some((").append(decodedNames.mkString(",")).append("))\n")
    sb.append("      case _ => None\n")
    sb.append("    }\n")
    sb.append("  }\n")
    sb.toString
  }
}
