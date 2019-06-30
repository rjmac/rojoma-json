package com.rojoma.json.v3

import scala.language.experimental.macros

import `-impl`.interpolation.JsonInterpolatorImpl
import ast.JValue

package object interpolation {
  implicit class jsonInterpolator(private val sc: StringContext) extends AnyVal {
    def j(pieces: Any*): JValue = macro JsonInterpolatorImpl.j
    def json(pieces: Any*): JValue = macro JsonInterpolatorImpl.j
  }
}
