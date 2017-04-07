package com.rojoma.json.v3.`-impl`.util

object EitherCompat {
  implicit class RightWrapper[K,V](val underlying: Right[K,V]) {
    def value = underlying match { case Right(v) => v }
  }

  implicit class LeftWrapper[K,V](val underlying: Left[K, V]) {
    def value = underlying match {case Left(v) => v }
  }
}
