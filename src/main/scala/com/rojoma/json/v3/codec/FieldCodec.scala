package com.rojoma.json.v3
package codec

object FieldCodec {
  def scalaEnumCodec[T <: Enumeration](e: T): FieldEncode[e.Value] with FieldDecode[e.Value] =
    new FieldEncode[e.Value] with FieldDecode[e.Value] {
      def encode(x: e.Value) = x.toString
      def decode(x: String) =
        try { Right(e.withName(x)) }
        catch { case _: NoSuchElementException => Left(DecodeError.InvalidField(x)) }
    }
}
