rojoma-json
===========

package com.rojoma.json.ast
---------------------------
 * `JValue`: An AST for JSON
   * `JAtom`
     * `JNull`
     * `JBoolean(boolean: Boolean)`
     * `JString(string: String)`
     * `JNumber`
       * `JIntegral(integral: Long)`
       * `JFloatingPoint(floatingPoint: Double)`
   * `JCompound`
     * `JArray(toSeq: scala.collection.Seq[JValue])`
     * `JObject(data: scala.collection.Map[String, JValue])`

The `JCompound` classes extend `Iterable` and have convenience methods
that make them act like `Seq` and `Map` respectively, but are not
themselves actually `Seq`s or `Map`s.  Use the `toSeq`, or `data` or
`toMap`, method to get a real one.

All `JValue`s have a `cast[T]` method that can be used to safely
downcast to a more specific type.

package com.rojoma.json.codec
-----------------------------
 * `JsonCodec[T]`: a typeclass for converting objects to/from JSON
   * `encode(x: T): JValue`
   * `decode(x: JValue): Option[T]`

The following types have implicit codecs in `JsonCodec`'s compantion:

 * `String`
 * `Boolean`
 * all subclasses of `JValue`
 * Any subclass of `Seq[T]` if `T` has a `JsonCodec`
 * Any subclass of `Map[String, T]` if `T` has a `JsonCodec`
 * `java.util.List[T]` if `T` has a `JsonCodec`
 * `java.util.Map[String, T]` if `T` has a `JsonCodec`

I am not yet certain what the best way to handle number is; namely
whether it is better to match strictly (so that a match fails if the
number in the JSON is not representable as the requested type) or
loosely (so that numbers may be rounded or otherwise mangled).

package com.rojoma.json.io
--------------------------
 * `JsonReader`: Convert character data to `JValues`
 * `JsonWriter`: Convert `JValues` to character data
   * `CompactJsonWriter`
   * `PrettyJsonWriter`

The two concrete `JsonWriter` classes have `toWriter` and `toString`
convenience methods on their companion objects.  `JsonReader`,
similarly, has `fromString` and `fromWriter`.  Neither `JsonReader`
nor the `JsonWriter`s make any effort to minimize the number of calls
to `read()` or `write()` on the IO handle they're given, so it is
probably a good idea to ensure that it is buffered.

package com.rojoma.json.matcher
-------------------------------
 * `OptPattern`: The base class of all `Pattern`s, plus `POption`
   * `Pattern`: A specification for extracting data from `JValue`s
     * `Literal(x: JValue)`: match a literal value
     * `FLiteral(f: JValue => Boolean)`: conditionally match a value
     * `PArray(subpatterns: Pattern*)`: match an array containing values that match a series of patterns
     * `PObject(subpatterns: (String, OptPattern)*)`: match an object containing fields that match patterns
     * `FirstOf(subpatterns: Pattern*)`: try to match a series of patterns in turn
     * `AllOf(subpatterns: OptPattern*)`: match a series of patterns in turn
     * `Variable[T : JsonCodec]`: match a value of type `T`
   * `POption(subpattern: Pattern)`: Optionally match a pattern.  Only valid in a `PObject` and `AllOf`.

These are probably best understood with a simple example:

```scala
val channel = Variable[String]() // Could be anything with a JsonCodec instance
val text = Variable[String]()
val ChatPattern =
  PObject("command" -> "chat",
          "to" -> channel,
          "message" -> text)
val JoinPattern =
  PObject("command" -> "join",
          "channel" -> channel)
val LeavePattern =
  PObject("command" -> "leave",
          "channel" -> channel,
          "message" -> POption(text))

def process(message: JValue) = message match {
  case ChatPattern(results) =>
    sendText(channel(results), text(results))
  case JoinPattern(results) =>
    joinChannel(channel(results))
  case LeavePattern(results) =>
    departChannel(channel(results), text.get(results))
  case _ =>
    error("unknown command", message)
}
```

`OptPattern`'s companion contains implicit conversions from various
literal forms into `Pattern`s.

Information is extracted by means of `Variable` patterns.  `Variable`s
can be created with the `apply` method on the companion object.  They
are typed and will only succeed in matching if the value at their
position is of the correct type.  If a single `Variable` appears more
than once in a `Pattern`, it is not an error, but all appearances must
match the same value.

Any object with an implicit `JsonCodec` instance in scope can be
automatically coerced to a literal `Pattern`.

The result of a match is an opaque object which can be given to a
`Variable` to extract the data, either by applying the `Variable` like
a function or calling its `get` method.  `Variables` and `Pattern`s
themselves are immutable.

`PObject` will accepts multiple patterns for a single field; all must
accept for the containing `PObject` to accept.  If the target of a
field is marked optional by wrapping it in a `POption`, it either must
match the subpattern or not appear at all.  To tolerate random
unparsable data in a field, use FirstOf with a final branch that
accepts anything.

In `AllOf`, if a field is marked with POption, the value under
consideration is allowed to _not_ match that particular subpattern.
In this context, `POption(p)` is a shorthand for `FirstOf(p, Variable[JValue]())`.

Custom matchers can be defined by subclassing `Pattern` and
implementing the method `evaluate(x: JValue, environment: Pattern.Results): Option[Pattern.Results]`.

package com.rojoma.json.util
----------------------------
Utility operations that combine parts from other packages.  Currently
the only member of this package is the object `JsonUtil` which contains
convenience methods for moving data all the way between character data
and usable objects.
