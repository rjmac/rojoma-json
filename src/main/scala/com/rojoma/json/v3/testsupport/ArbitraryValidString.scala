package com.rojoma.json.v3
package testsupport

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

object ArbitraryValidString {
  // Strings with surrogate characters that make valid surrogate pairs
  val ArbitraryValidString: Arbitrary[String] = {
    val lowSurrogate = Gen.choose(Character.MIN_LOW_SURROGATE, Character.MAX_LOW_SURROGATE).map(_.toChar)

    val notLowSurrogate = Gen.frequency(
      (Character.MIN_LOW_SURROGATE.toInt - Char.MinValue.toInt, Gen.choose(Char.MinValue.toInt, Character.MIN_LOW_SURROGATE - 1)),
      (Char.MaxValue - Character.MAX_LOW_SURROGATE, Gen.choose(Character.MAX_LOW_SURROGATE + 1, Char.MaxValue.toInt))
    ).map(_.toChar)

    val validCodePoint = notLowSurrogate flatMap { a =>
      if(a.isHighSurrogate) lowSurrogate map { b => new String(Array(a, b)) }
      else a.toString
    }

    Arbitrary(Gen.containerOf[List, String](validCodePoint) map (_.mkString))
  }
}
