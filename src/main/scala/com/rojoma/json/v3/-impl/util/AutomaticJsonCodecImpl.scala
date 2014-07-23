package com.rojoma.json.v3
package `-impl`.util

import java.nio.charset.StandardCharsets

import MacroCompat._
import util._

abstract class AutomaticJsonCodecImpl extends MacroCompat {
  val c: Context
  import c.universe._

  val bs: Seq[c.Expr[Any]]

  val requestType: Class[_]

  def defaultCompanion(cls: ClassDef) =
    if(cls.mods.hasFlag(Flag.CASE)) {
      // if the primary ctor has one argument list, we want to
      // extend AbstractFunctionN and Serializable, otherwise
      // just Serializable.

      // I'm pretty sure the primary ctor will always be first..
      val ctorInfo = cls.impl.children.collectFirst {
        case DefDef(mods, name, _, pparams, _, _) if name.decodedName.toString == "<init>" =>
          pparams
      }

      val parents = ctorInfo match {
        case Some(List(paramList)) if paramList.length < 23 =>
          val afn = toTypeName("AbstractFunction" + paramList.length)
          val params = paramList.map { param =>
            val Scala = toTermName("Scala") // 2.10 grr
            val Repeated = toTypeName("<repeated>") // 2.10 grr
            param.tpt match {
              case AppliedTypeTree(Select(Select(Ident(termNames.ROOTPKG), Scala), Repeated), List(t)) =>
                tq"_root_.scala.Seq[$t]"
              case other =>
                other
            }
          }
          List(tq"_root_.scala.runtime.$afn[..$params, ${cls.name}]", tq"_root_.scala.Serializable")
        case _ => List(tq"_root_.scala.Serializable")
      }

      // I REALLY need to find a way to add modifiers after the fact!
      if(cls.mods.hasFlag(Flag.PRIVATE)) {
        q"""private object ${cls.name.toTermName} extends ..$parents {
              override def toString = ${cls.name.decodedName.toString}
            }"""
      } else if(cls.mods.hasFlag(Flag.PROTECTED)) {
        q"""protected object ${cls.name.toTermName} extends ..$parents {
              override def toString = ${cls.name.decodedName.toString}
            }"""
      } else {
        q"""object ${cls.name.toTermName} extends ..$parents {
              override def toString = ${cls.name.decodedName.toString}
            }"""
      }
    } else {
      if(cls.mods.hasFlag(Flag.PRIVATE)) {
        q"private object ${cls.name.toTermName} {}"
      } else if(cls.mods.hasFlag(Flag.PROTECTED)) {
        q"protected object ${cls.name.toTermName} {}"
      } else {
        q"object ${cls.name.toTermName} {}"
      }
    }

  val (cls, companion) = bs.map(_.tree) match {
    case List(cls: ClassDef) => (cls, defaultCompanion(cls))
    case List(cls: ClassDef, companion: ModuleDef) => (cls, companion)
    case _ =>
      c.abort(c.enclosingPosition, s"Invalid target for ${requestType.getName}")
  }

  // Hm.  I'm not sure a freshName would be guaranteed to be stable.
  // So we'll do this instead:
  def sha1sum(s: String) = {
    val md = java.security.MessageDigest.getInstance("SHA1")
    md.digest(s.getBytes(StandardCharsets.UTF_8)).map(_.toInt & 0xff).map("%02x".format(_)).mkString
  }
  val generatedName = toTermName("automatically generated json codec " + sha1sum(cls.name.encodedName.toString))

  def augmentedCompanion(t: Tree) =
    ModuleDef(companion.mods, companion.name, Template(companion.impl.parents, companion.impl.self, companion.impl.body :+ q"implicit val $generatedName = $t"))

  def codec = {
    val newCompanion = augmentedCompanion(q"_root_.com.rojoma.json.v3.util.AutomaticJsonCodecBuilder.apply[${cls.name}]")
    val tree = q"$cls; $newCompanion"
    // println(tree)
    c.Expr[Any](tree)
  }

  def encode = {
    val newCompanion = augmentedCompanion(q"_root_.com.rojoma.json.v3.util.AutomaticJsonEncodeBuilder.apply[${cls.name}]")
    val tree = q"$cls; $newCompanion"
    // println(tree)
    c.Expr[Any](tree)
  }

  def decode = {
    val newCompanion = augmentedCompanion(q"_root_.com.rojoma.json.v3.util.AutomaticJsonDecodeBuilder.apply[${cls.name}]")
    val tree = q"$cls; $newCompanion"
    // println(tree)
    c.Expr[Any](tree)
  }
}

object AutomaticJsonCodecImpl {
  def codec(ctx: Context)(annottees: ctx.Expr[Any]*): ctx.Expr[Any] = {
    val b = new {
      val c: ctx.type = ctx
      val bs = annottees
      val requestType = classOf[AutomaticJsonCodec]
    } with AutomaticJsonCodecImpl

    b.codec
  }

  def encode(ctx: Context)(annottees: ctx.Expr[Any]*): ctx.Expr[Any] = {
    val b = new {
      val c: ctx.type = ctx
      val bs = annottees
      val requestType = classOf[AutomaticJsonEncode]
    } with AutomaticJsonCodecImpl

    b.encode
  }

  def decode(ctx: Context)(annottees: ctx.Expr[Any]*): ctx.Expr[Any] = {
    val b = new {
      val c: ctx.type = ctx
      val bs = annottees
      val requestType = classOf[AutomaticJsonDecode]
    } with AutomaticJsonCodecImpl

    b.decode
  }
}
