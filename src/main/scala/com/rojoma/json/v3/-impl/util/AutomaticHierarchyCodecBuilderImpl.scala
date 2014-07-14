package com.rojoma.json.v3
package `-impl`.util

import util._
import codec._
import MacroCompat._

abstract class AutomaticHierarchyCodecBuilderImpl[T <: AnyRef] extends MacroCompat with MacroCommon {
  val c: Context
  import c.universe._

  implicit def Ttag: c.WeakTypeTag[T]
  private lazy val T = weakTypeOf[T]
  private lazy val Tname = TypeTree(T)

  lazy val subclasses = locally {
    val cls = T.typeSymbol.asClass
    if(!cls.isSealed) c.abort(T.typeSymbol.pos, "Class must be sealed for use in an automatic hiearchy codec")
    cls.typeSignature
    if(cls.knownDirectSubclasses.isEmpty) c.abort(T.typeSymbol.pos, "Class has no known subclasses; cannot build a hierarchy codec (note: see https://issues.scala-lang.org/browse/SI-7046, https://issues.scala-lang.org/browse/SI-7588)")
    cls.knownDirectSubclasses
  }

  def tagOf(thing: Symbol): String =
    thing.annotations.find { ann => isType(ann.tree.tpe, typeOf[JsonTag]) } match {
      case Some(ann) =>
        findValue(ann) match {
          case Some(tag: String) => tag
          case _ => c.abort(thing.pos, "No value for JsonTag annotation?")
        }
      case None =>
        c.abort(thing.pos, "Must provide a JsonTag to use in a hierarchy encode/decode")
    }

  def tagged(base: Tree): Tree = {
    val (branches, _) =
      subclasses.foldLeft((base, Set.empty[String])) { (acc, sc) =>
        val (tree, tags) = acc
        val tag = tagOf(sc)
        if(tags contains tag) c.abort(sc.pos, "Tag already attached do a different branch of the hierarchy")
        (q"$tree.branch[$sc]($tag)", tags + tag)
      }
    q"$branches.build"
  }

  def tagless(base: Tree): Tree = {
    val branches =
      subclasses.foldLeft(base) { (tree, sc) =>
        q"$tree.branch[$sc]"
      }
    q"$branches.build"
  }

  def encodeTagged(tagType: c.Expr[TagType]): c.Expr[JsonEncode[T]] = {
    val tree = tagged(q"_root_.com.rojoma.json.v3.util.SimpleHierarchyEncodeBuilder[$Tname]($tagType)")
    // println(tree)
    c.Expr[JsonEncode[T]](tree)
  }


  def encodeTagless(tagType: c.Expr[NoTag]): c.Expr[JsonEncode[T]] = {
    val tree = tagless(q"_root_.com.rojoma.json.v3.util.SimpleHierarchyEncodeBuilder[$Tname]($tagType)")
    // println(branches)
    c.Expr[JsonEncode[T]](tree)
  }

  def decodeTagged(tagType: c.Expr[TagType]): c.Expr[JsonDecode[T]] = {
    val tree = tagged(q"_root_.com.rojoma.json.v3.util.SimpleHierarchyDecodeBuilder[$Tname]($tagType)")
    // println(tree)
    c.Expr[JsonDecode[T]](tree)
  }


  def decodeTagless(tagType: c.Expr[NoTag]): c.Expr[JsonDecode[T]] = {
    val tree = tagless(q"_root_.com.rojoma.json.v3.util.SimpleHierarchyDecodeBuilder[$Tname]($tagType)")
    // println(branches)
    c.Expr[JsonDecode[T]](tree)
  }

  def codecTagged(tagType: c.Expr[TagType]): c.Expr[JsonEncode[T] with JsonDecode[T]] = {
    val tree = tagged(q"_root_.com.rojoma.json.v3.util.SimpleHierarchyCodecBuilder[$Tname]($tagType)")
    // println(tree)
    c.Expr[JsonEncode[T] with JsonDecode[T]](tree)
  }


  def codecTagless(tagType: c.Expr[NoTag]): c.Expr[JsonEncode[T] with JsonDecode[T]] = {
    val tree = tagless(q"_root_.com.rojoma.json.v3.util.SimpleHierarchyCodecBuilder[$Tname]($tagType)")
    // println(branches)
    c.Expr[JsonEncode[T] with JsonDecode[T]](tree)
  }
}

object AutomaticHierarchyCodecBuilderImpl {
  def encodeTagged[T <: AnyRef : ctx.WeakTypeTag](ctx: Context)(tagType: ctx.Expr[TagType]): ctx.Expr[JsonEncode[T]] = {
    val b = new AutomaticHierarchyCodecBuilderImpl[T] {
      val c = ctx
      val Ttag = implicitly[ctx.WeakTypeTag[T]].asInstanceOf[c.WeakTypeTag[T]]
    }
    b.encodeTagged(tagType.asInstanceOf[b.c.Expr[TagType]]).asInstanceOf[ctx.Expr[JsonEncode[T]]]
  }

  def encodeTagless[T <: AnyRef : ctx.WeakTypeTag](ctx: Context)(tagType: ctx.Expr[NoTag]): ctx.Expr[JsonEncode[T]] = {
    val b = new AutomaticHierarchyCodecBuilderImpl[T] {
      val c = ctx
      val Ttag = implicitly[ctx.WeakTypeTag[T]].asInstanceOf[c.WeakTypeTag[T]]
    }
    b.encodeTagless(tagType.asInstanceOf[b.c.Expr[NoTag]]).asInstanceOf[ctx.Expr[JsonEncode[T]]]
  }

  def decodeTagged[T <: AnyRef : ctx.WeakTypeTag](ctx: Context)(tagType: ctx.Expr[TagType]): ctx.Expr[JsonDecode[T]] = {
    val b = new AutomaticHierarchyCodecBuilderImpl[T] {
      val c = ctx
      val Ttag = implicitly[ctx.WeakTypeTag[T]].asInstanceOf[c.WeakTypeTag[T]]
    }
    b.decodeTagged(tagType.asInstanceOf[b.c.Expr[TagType]]).asInstanceOf[ctx.Expr[JsonDecode[T]]]
  }

  def decodeTagless[T <: AnyRef : ctx.WeakTypeTag](ctx: Context)(tagType: ctx.Expr[NoTag]): ctx.Expr[JsonDecode[T]] = {
    val b = new AutomaticHierarchyCodecBuilderImpl[T] {
      val c = ctx
      val Ttag = implicitly[ctx.WeakTypeTag[T]].asInstanceOf[c.WeakTypeTag[T]]
    }
    b.decodeTagless(tagType.asInstanceOf[b.c.Expr[NoTag]]).asInstanceOf[ctx.Expr[JsonDecode[T]]]
  }

  def codecTagged[T <: AnyRef : ctx.WeakTypeTag](ctx: Context)(tagType: ctx.Expr[TagType]): ctx.Expr[JsonEncode[T] with JsonDecode[T]] = {
    val b = new AutomaticHierarchyCodecBuilderImpl[T] {
      val c = ctx
      val Ttag = implicitly[ctx.WeakTypeTag[T]].asInstanceOf[c.WeakTypeTag[T]]
    }
    b.codecTagged(tagType.asInstanceOf[b.c.Expr[TagType]]).asInstanceOf[ctx.Expr[JsonEncode[T] with JsonDecode[T]]]
  }

  def codecTagless[T <: AnyRef : ctx.WeakTypeTag](ctx: Context)(tagType: ctx.Expr[NoTag]): ctx.Expr[JsonEncode[T] with JsonDecode[T]] = {
    val b = new AutomaticHierarchyCodecBuilderImpl[T] {
      val c = ctx
      val Ttag = implicitly[ctx.WeakTypeTag[T]].asInstanceOf[c.WeakTypeTag[T]]
    }
    b.codecTagless(tagType.asInstanceOf[b.c.Expr[NoTag]]).asInstanceOf[ctx.Expr[JsonEncode[T] with JsonDecode[T]]]
  }
}
