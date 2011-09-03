mappings in (Compile, packageSrc) <++= (sourceManaged in Compile, managedSources in Compile) map { (base, srcs) =>
  import Path.{flat, relativeTo}
  println(srcs)
  srcs x (relativeTo(base) | flat)
}

sourceGenerators in Compile <+= (sourceManaged in Compile) map { managedRoot =>
  val outputDir = managedRoot / "simple-json-codec-builder"
  def genUsing(n: Int): String = {
    val names = (0 until n).map(i => ('A' + i).toChar).toIndexedSeq
    val sb = new StringBuilder
    sb.append("def gen").append(names.map(_ + ": JsonCodecOrOption: Manifest").mkString("[", ", ", "]")).append(names.map(a => "n" + a + ": String, a" + a +" : (TT => " + a + ")").mkString("(",", ",")")).append(": JsonCodec[TT] = {\n")
    sb.append("  val ctor = findCtor(t[TT])").append(names.map(a => "t[" + a + "]").mkString("(",", ",")")).append("\n")
    for(a <- names) {
      sb.append("  val (assign" + a + ", retrieve" + a + ", patTarget" + a + ") = extract[" + a + "]\n")
    }
    sb.append("  val pattern = PObject").append(names.map(a => "n" + a + " -> patTarget" + a).mkString("(",", ",")")).append("\n")
    sb.append("  new JsonCodec[TT] {\n")
    sb.append("    def encode(x: TT) = pattern.generate").append(names.map(a => "assign" + a + "(a" + a + "(x))").mkString("(",", ",")")).append("\n")
    sb.append("    def decode(x: JValue): Option[TT] =\n")
    sb.append("      pattern.matches(x) map { results =>\n")
    sb.append("        val params = Array").append(names.map(a => "retrieve" + a + "(results)").mkString("(",", ",")")).append("\n")
    sb.append("        try {\n")
    sb.append("          ctor.newInstance(params: _*).asInstanceOf[TT]\n")
    sb.append("        } catch {\n")
    sb.append("          case e: java.lang.reflect.InvocationTargetException => throw e.getCause\n")
    sb.append("        }\n")
    sb.append("      }\n")
    sb.append("  }\n")
    sb.append("}\n")
    sb.toString
  }
  outputDir.mkdirs()
  val outfile = outputDir / "SimpleJsonCodecBuilder.scala"
  val f = new java.io.FileWriter(outfile)
  try {
    f.write("""package com.rojoma.json
package util
//
import ast._
import codec._
import matcher._
//
import com.rojoma.`json-impl`.util._
//
object SimpleJsonCodecBuilder {
  private def t[A: Manifest]: Class[_] = manifest[A].erasure
  //
  private def findCtor(baseClass: Class[_])(fieldClasses: Class[_]*) = {
    baseClass.getConstructor(fieldClasses : _*)
  }
  // the horror, the horror
  private def extract[A](implicit jcooA: JsonCodecOrOption[A]) = jcooA match {
    case jc: JsonCodecVersion[_] =>
      val varA = Variable[jc.RealType]()(jc.codec)
      val assignA = (varA := _).asInstanceOf[Any => Pattern.Results => Pattern.Results]
      val retrieveA = varA(_: Pattern.Results).asInstanceOf[AnyRef]
      (assignA, retrieveA, varA)
    case o: OptionVersion[_] =>
      val varA = Variable[o.RealType]()(o.codec)
      val assignA = (varA :=? _).asInstanceOf[Any => Pattern.Results => Pattern.Results]
      val retrieveA = varA.get(_: Pattern.Results).asInstanceOf[AnyRef]
      (assignA, retrieveA, POption(varA))
  }
  //
  class FixedSimpleJsonCodecBuilder[TT: Manifest] {
""")
    for(i <- 1 to 22) f.write(genUsing(i))
    f.write("""  }
  //
  def apply[TT: Manifest] = new FixedSimpleJsonCodecBuilder[TT]
}
""")
  } finally {
    f.close()
  }
  Seq(outfile)
}
