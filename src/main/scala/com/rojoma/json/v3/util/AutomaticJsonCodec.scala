package com.rojoma.json.v3
package util

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

import `-impl`.util.AutomaticJsonCodecImpl

class AutomaticJsonCodec extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AutomaticJsonCodecImpl.codec
}

class AutomaticJsonEncode extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AutomaticJsonCodecImpl.encode
}

class AutomaticJsonDecode extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AutomaticJsonCodecImpl.decode
}
