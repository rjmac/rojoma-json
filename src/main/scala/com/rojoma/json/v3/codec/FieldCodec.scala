package com.rojoma.json.v3
package codec

object FieldCodec {
  def scalaEnumCodec[T <: Enumeration](enum: T): FieldEncode[enum.Value] with FieldDecode[enum.Value] =
    new FieldEncode[enum.Value] with FieldDecode[enum.Value] {
      def encode(x: enum.Value) = x.toString
      def decode(x: String) =
        try { Right(enum.withName(x)) }
        catch { case _: NoSuchElementException => Left(DecodeError.InvalidField(x)) }
    }
}
