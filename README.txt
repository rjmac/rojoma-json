JValue: An AST for JSON
  JAtom
    JNull
    JBoolean(boolean: Boolean)
    JString(string: String)
    JNumber
      JIntegral(integral: Long)
      JFloatingPoint(floatingPoint: Double)
  JCompound
    JArray(toSeq: Seq[JValue])
    JObject(data: Map[String, JValue])

JsonCodec[T] : a typeclass for converting objects to/from JSON
  encode(x: T): JValue
  decode(x: JValue): Option[T]

JsonCodecs : implicit vals and defs JsonCodec

JsonReader : Convert character data to JValues
JsonWriter : Convert JValues to character data
  CompactJsonWriter
  PrettyJsonWriter

OptPattern : A system for extracting data from JSON in a declarative fashion
  Pattern
    Literal(x: JValue) : match a literal value
    FValue(f: JValue => Boolean) : conditionally match a value
    PArray(subpatterns: Pattern*) : match an array containing values that match a series of patterns
    PObject(subpatterns: (String, OptPattern)*) : match an object containing fields that match patterns
    FirstOf(subpatterns: Pattern*) : Try to match a series of patterns in turn
    Variable[T] : Try to match a value of type T; either a "raw" JValue or a "cooked" product of a JsonCodec
  POption(subpattern: Pattern) : Optionally match a pattern.  Only valid in PObject
