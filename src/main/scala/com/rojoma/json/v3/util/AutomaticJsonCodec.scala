package com.rojoma.json.v3
package util

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

import `-impl`.util.MacroCompat.compileTimeOnly
import `-impl`.util.AutomaticJsonCodecImpl


/** Add implicit [[com.rojoma.json.v3.codec.JsonEncode]] and [[com.rojoma.json.v3.codec.JsonDecode]] instances to the annotated class.
  *
  * @note when using this macro, annotated case classes must specify context bounds explicitly rather than using ":" syntax, due to [[https://github.com/scala/bug/issues/10589]]
  */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class AutomaticJsonCodec extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AutomaticJsonCodecImpl.codec
}

/** Add an implicit [[com.rojoma.json.v3.codec.JsonEncode]] instance to the annotated class.
  *
  * @note when using this macro, annotated case classes must specify context bounds explicitly rather than using ":" syntax, due to [[https://github.com/scala/bug/issues/10589]]
  */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class AutomaticJsonEncode extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AutomaticJsonCodecImpl.encode
}

/** Add an implicit [[com.rojoma.json.v3.codec.JsonDecode]] instance to the annotated class.
  *
  * @note when using this macro, annotated case classes must specify context bounds explicitly rather than using ":" syntax, due to [[https://github.com/scala/bug/issues/10589]]
  */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class AutomaticJsonDecode extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AutomaticJsonCodecImpl.decode
}
