package com.rojoma.json.v3
package util

import codec.{FieldEncode, FieldDecode, DecodeError}

private[util] class WrapperFieldDecodeImpl[T](wrap: String => T) extends FieldDecode[T] {
  def decode(j: String): Either[DecodeError.InvalidField, T] = {
    try {
      Right(wrap(j))
    } catch {
      case e: IllegalArgumentException =>
        Left(DecodeError.InvalidField(j))
    }
  }
}

object WrapperFieldCodec {
  def apply[T](wrap: String => T, unwrap: T => String): FieldEncode[T] with FieldDecode[T] =
    new WrapperFieldDecodeImpl(wrap) with FieldEncode[T] {
      def encode(x: T) = unwrap(x)
    }
}

object WrapperFieldEncode {
  def apply[T](unwrap: T => String): FieldEncode[T] =
    new FieldEncode[T] {
      def encode(x: T) = unwrap(x)
    }
}

object WrapperFieldDecode {
  def apply[T](wrap: String => T): FieldDecode[T] =
    new WrapperFieldDecodeImpl(wrap)
}
