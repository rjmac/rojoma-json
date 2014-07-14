import sbt._

object SimpleJsonCodecBuilderBuilder extends (File => Seq[File]) {
  def apply(root: File): Seq[File] = {
    val outputDir = root / "com" / "rojoma" / "json" / "v3" / "util"
    outputDir.mkdirs()
    val outputFile = outputDir / "SimpleJsonCodecBuilder.scala"
    val f = new java.io.OutputStreamWriter(new java.io.FileOutputStream(outputFile), "UTF-8")

    try {
      f.write("""package com.rojoma.json.v3
package util

import scala.reflect.ClassTag

import ast._
import codec._
import matcher._

import `-impl`.util._

object SimpleJsonCodecBuilder {
  private def t[A](implicit m: ClassTag[A]): Class[_] = m.runtimeClass

  private def findCtor(baseClass: Class[_])(fieldClasses: Class[_]*) = {
    baseClass.getConstructor(fieldClasses : _*)
  }

  // the horror, the horror
  private def extract[A](implicit jcooA: JsonCodecOrOption[A]) = jcooA match {
    case jc: JsonCodecVersion[A] =>
      val varA = Variable[jc.RealType]()(jc.dec, jc.enc)
      val assignA = (varA := _).asInstanceOf[Any => Pattern.Results => Pattern.Results]
      val retrieveA = varA(_: Pattern.Results).asInstanceOf[AnyRef]
      (assignA, retrieveA, varA)
    case o: OptionVersion[A] =>
      val varA = Variable[o.RealType]()(o.dec, o.enc)
      val assignA = (varA :=? _).asInstanceOf[Any => Pattern.Results => Pattern.Results]
      val retrieveA = varA.get(_: Pattern.Results).asInstanceOf[AnyRef]
      (assignA, retrieveA, POption(varA).orNull)
  }

  class FixedSimpleJsonCodecBuilder[TT: ClassTag] {
""")
    for(i <- 1 to 22) f.write(genUsing(i))
    f.write("""  }

  def apply[TT: ClassTag] = new FixedSimpleJsonCodecBuilder[TT]
}
""")
    } finally {
      f.close()
    }
    Seq(outputFile)
  }

  def genUsing(n: Int): String = {
    def nameBase(i: Int) = ('A' + i).toChar

    val sb = new StringBuilder

    def typeName(i: Int) = nameBase(i).toString
    def fieldName(i: Int) = "f" + nameBase(i)
    def accessorName(i: Int) = "a" + nameBase(i)
    def assignName(i: Int) = "assign" + nameBase(i)
    def retrieveName(i: Int) = "retrieve" + nameBase(i)
    def patTargetName(i: Int) = "patTarget" + nameBase(i)

    def mapNames1[T](a: Int => String)(f: String => T) = (0 until n).map(a).map(f)
    def mapNames2[T](a: Int => String, b: Int => String)(f: (String, String) => T) = (0 until n).map(x => (a(x), b(x))).map(f.tupled)
    def mapNames3[T](a: Int => String, b: Int => String, c: Int => String)(f: (String, String, String) => T) = (0 until n).map(x => (a(x), b(x), c(x))).map(f.tupled)
    def foreachNames4(a: Int => String, b: Int => String, c: Int => String, d: Int => String)(f: (String, String, String, String) => Any) = (0 until n).map(x => (a(x), b(x), c(x), d(x))).foreach(f.tupled)

    sb.append("def build").append(mapNames1(typeName)(_ + ": JsonCodecOrOption: ClassTag").mkString("[", ", ", "]")).append(mapNames3(fieldName, accessorName, typeName)(_ + ": String, " + _ + ": TT => " + _).mkString("(",", ",")")).append(": JsonEncode[TT] with JsonDecode[TT] = {\n")
    sb.append("  val ctor = findCtor(t[TT])").append(mapNames1(typeName)("t[" + _ + "]").mkString("(",", ",")")).append("\n")
    foreachNames4(assignName, retrieveName, patTargetName, typeName) { (a, r, p, t) =>
      sb.append("  val (" + a + ", " + r + ", " + p + ") = extract[" + t + "]\n")
    }
    sb.append("  val pattern = PObject").append(mapNames2(fieldName, patTargetName)(_ + " -> " + _).mkString("(",", ",")")).append("\n")
    sb.append("  new JsonEncode[TT] with JsonDecode[TT] {\n")
    sb.append("    def encode(x: TT) = pattern.generate").append(mapNames2(assignName, accessorName)(_ + "(" + _ + "(x))").mkString("(",", ",")")).append("\n")
    sb.append("    def decode(x: JValue): Either[DecodeError, TT] =\n")
    sb.append("      pattern.matches(x) match {\n")
    sb.append("        case Right(results) =>\n")
    sb.append("          val params = Array").append(mapNames1(retrieveName)(_ + "(results)").mkString("(",", ",")")).append("\n")
    sb.append("          try {\n")
    sb.append("            Right(ctor.newInstance(params: _*).asInstanceOf[TT])\n")
    sb.append("          } catch {\n")
    sb.append("            case e: java.lang.reflect.InvocationTargetException => throw e.getCause\n")
    sb.append("          }\n")
    sb.append("        case Left(err) =>\n")
    sb.append("          Left(err)\n")
    sb.append("      }\n")
    sb.append("    def acceptTypes = CommonAcceptTypes.justJObject\n")
    sb.append("  }\n")
    sb.append("}\n")
    sb.toString
  }
}

