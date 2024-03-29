# rojoma-json

## Getting it

Starting with version 2.0.0, rojoma-json is published on Maven
central, so setting up SBT is as simple as

```scala
libraryDependencies += "com.rojoma" %% "rojoma-json-v3" % "3.15.0"
```

While for Maven, the pom snippet is:

```xml
<dependencies>
  <dependency>
    <groupId>com.rojoma</groupId>
    <artifactId>rojoma-json-v3_${scala.version}</artifactId>
    <version>3.15.0</version>
  </dependency>
</dependencies>
```

rojoma-json-v3 is published for Scala version 2.10, 2.11, 2.12, and 2.13.

## Documentation

### package com.rojoma.json.v3.ast

 * `JValue`: An AST for JSON
    * `JAtom`
       * `JNull`
       * `JBoolean(boolean: Boolean)`
       * `JString(string: String)`
       * `JNumber`
    * `JCompound`
       * `JArray(toSeq: scala.collection.Seq[JValue])`
       * `JObject(fields: scala.collection.Map[String, JValue])`

The `JCompound` classes extend `Iterable` and have convenience methods
that make them act like `Seq` and `Map` respectively, but are not
themselves actually `Seq`s or `Map`s.  Use the `toSeq`, or `fields` or
`toMap`, method to get a real one.

`JNumber` is not a case class; it is an abstract class whose
implementation varies depending on how it was constructed.  Use the
various `toX` methods to access its value.

All `JValue`s have a `cast[T]` method that can be used to safely
downcast to a more specific type.

There is also support for "dynamically typed" access to JValues:

```scala
someJValue = j"{outer : {inner : [0,1,2,3]}}"
someJValue.dyn.outer.inner(2).?             // returns Right(JNumber(2))
someJValue.dyn.outer("inner")(2).?          // returns Right(JNumber(2))
someJValue.dyn.outer.inner(2).!             // returns JNumber(2)
someJValue.dyn.outer.nonexistant.inner(2).? // returns Left(DecodeError.MissingField("nonexistant", .outer))
someJValue.dyn.outer.nonexistant.inner(2).! // throws a NoSuchElementException
```

This is implemented via scala's `Dynamic` trait; as a result,
`applyDynamic`, `selectDynamic`, and `apply`, plus the methods on
`Object`, will resolve to real methods instead of path elements.

### package com.rojoma.json.v3.codec

 * `JsonEncode[T]`: a typeclass for converting objects to `JValue`s
 * `JsonDecode[T]`: a typeclass for converting objects from `JValue`s
 * `FieldEncode[T]`: a typeclass for converting objects to object keys
 * `FieldDecode[T]`: a typeclass for converting objects from object keys

Encoding is assumed to always succeed; decoding can fail, returning a
`DecodeError` containing the cause of the failure and the path at
which the failure happened.

The following types have implicit codecs in `JsonEncode`'s and
`JsonDecode`'s companions:

 * `String`
 * `Boolean`
 * Numeric types, including `BigInt`, `BigDecimal`, and their `java.math` counterparts
 * `JValue` and all its subclasses
 * Any `S[T] <: Seq[T]` if `T` has a `JsonCodec` and `S` has an implicit `CanBuild`
 * Any `S[T] <: Set[T]` if `T` has a `JsonCodec` and `S` has an implicit `CanBuild`
 * Any `M[T, U] <: Map[T,U]` if `T` has a `FieldCodec`, `U` has a `JsonCodec` and `M` has an implicit `CanBuild`
 * `Either` (biased on decoding to `Right`)
 * `Unit`
 * Tuples up to `Tuple22`
 * `java.util.List[T]` if `T` has a `JsonCodec`
 * `java.util.Set[T]` if `T` has a `JsonCodec`
 * `java.util.Map[T, U]` if `T` has a `FieldCodec` and `U` has a `JsonCodec`
 * Java enumerations
 * `java.util.UUID`
 * `java.net.URL`

The "atomic" ones (`String`, `Boolean`, numbers, enums, `UUID`, and
`URL`) also have field codecs in `FieldEncode` and `FieldDecode`.

Numeric codecs are "lenient" -- that is, if a number is out of range
of the requested type, it undergoes the normal truncation
`BigDecimal.toXXX` does.  If this is not desired, request a
`BigDecimal` and use the `.toXXXExact` alternatives.

In addition to the implicit codecs, Scala `Enumeration`s can have
codecs automatically generated for them via the non-implicit methods
`JsonEncode.scalaEnumEncode` and `JsonDecode.scalaEnumDecode` (or the
convenience `JsonCodec.scalaEnumCodec` which defines both).  These
methods all take the enumeration's container object as a parameter.

The codecs' companion objects themselves can be used as values that
represent the result of an implicit search.  That is:

```scala
JsonEncode[T] === implicitly[JsonEncode[T]]
JsonDecode[T] === implicitly[JsonDecode[T]]
FieldEncode[T] === implicitly[FieldEncode[T]]
FieldDecode[T] === implicitly[FieldDecode[T]]
```

Default `JsonEncode`s will be as lazy as possible.  Modifying the
object that was encoded before serializing the resulting `JValue` or
calling `forced` on it has undefined behavior.

### package com.rojoma.json.v3.io

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

As extensions to the JSON specification, `JsonReader` accepts the
following:

 * single-quote delimited strings
 * unquoted object keys
 * Javascript-style comments

The only limits on the sizes of strings and depth of nesting are those
of the JVM.  Parsing is done recursively, and so stack space is the
limiting factor in nesting.  Numbers are restricted to those which can
fit in a Java `BigDecimal`.  The reader does validate surrogate pairs
and will replace stray halves with the Unicode `REPLACEMENT_CHARACTER`
character.  `JsonReader` also guarantees to read exactly as much as
necessary as to read a single `JValue` -- i.e., only to the closing
delimiter for objects, arrays, and strings, and one character past for
all other types of JSON datum.

Below the `JsonReader` level lives the `JsonEvent` level, which can be
used to implement streamed processing.  A `JsonReader` consumes an
`Iterator[JsonEvent]`; that iterator can itself be either a
`JsonEventIterator` (which consumes an `Iterator[JsonToken]`) or a
`FusedBlockJsonEventIterator` (which reads character data directly).
In the former case, `Iterator[JsonToken]` is provided by
`JsonTokenIterator` and `BlockJsonTokenIterator`.  Building on top of
the non-`Block` versions will ensure, at a performance penalty, that
no more is read from the underlying source of character data than is
necessary to read a complete JSON datum; the `Block` variants are
faster at the cost of reading more than that.  If you do not specify,
rojoma-json will always pick the non-`Block` versions when reading
from `Reader`s.

### package com.rojoma.json.v3.matcher

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

def leaveChannel(channelName: String, message: Option[String]): JValue =
  LeavePattern.generate(channel := channelName, text :=? message)
```

`OptPattern`'s companion contains implicit conversions from various
literal forms into `Pattern`s.

Information is extracted by means of `Variable` patterns.  `Variable`s
can be created with the `apply` method on the companion object.  They
are typed and will only succeed in matching if the value at their
position is of the correct type.  If a single `Variable` appears more
than once in a `Pattern`, it is not an error, but all appearances must
match the same value.

Any object with an implicit `JsonDecode` instance in scope can be
automatically coerced to a literal `Pattern`.  If there is also a
`JsonEncode` instance, that pattern can be used as a generator.

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
implementing the method `evaluate(x: JValue, environment: Pattern.Results): Either[DecodeError, Pattern.Results]`.

Most `Pattern`s can also be used to generate JSON using the `generate`
method, passing in a list of variable bindings in the form `variable := value` or,
if the variable occurs in an "optional" position (i.e., inside a `POption`
or `FirstOf`) `variable :=? optValue`, where `optValue` is an `Option`.

### package com.rojoma.json.v3.zipper

A zipper for navigating JSON.  There are five interfaces:

 * `JsonZipper`
    * `JAtomZipper`
    * `JArrayZipper`
    * `JObjectZipper`
 * `NothingZipper`

An array or object zipper may be acquired by calling `asArray` or
`asObject` on a generic zipper.

Each of the first five allows you to move `up`, to the `top` of the
object, or find the zipper's current `value`, `replace`, or `remove`
it.  In addition, the array and object zippers allow `replace`ing or
`remove`ing child elements.  All of the motion operators (except
`top`) return `Option`s; suffix the operator with `_!` to make it
return the value directly or throw a `NoSuchElementException` if the
motion is impossible.

The `NothingZipper` is special -- it is what is returned from removing
the current object.  With a `NothingZipper` you can either put a new
object in the hole it represents (via the `replace` method) or move
`up` or to the `top`.  Unlike the `JsonZipper` classes, when you have
nothing `top` might not return anything, since it is possible that the
root object is what was removed.

### package com.rojoma.json.v3.jpath

The `JPath` class is a simple wrapper over `JsonZipper`s for doing
"xpath-style" queries on a `JValue`.

```scala
// Find the first name of all users that live in Tucson
new JPath(myObject).down("users").*.having(_.down("city").where(_.here == JString("Tucson"))).down("firstName").finish
```

The result is a `Stream[JValue]`.  Currently `JPath` is a read-only
interface.

### package com.rojoma.json.v3.util

Utility operations that combine parts from other packages.  The main
member of this package is the object `JsonUtil` which contains
convenience methods for moving data all the way between character data
and usable objects. 

The package includes helpers for building `JsonEncode` and
`JsonDecode` instances.  For single classes, there is the macro-based
`AutomaticJsonCodecBuilder` (also `AutomaticJsonEncodeBuilder` and
`AutomaticJsonDecodeBuilder` for unidirectional conversion) which can
produce instances for case-like classes automatically, as well as
annotation macros `@AutomaticJsonCodec`, `@AutomaticJsonEncode` and
`@AutomaticJsonDecode`.  There is also the older
`SimpleJsonCodecBuilder` (with encode and decode variants) that
requires the programmer to say how to (de)construct objects.  They are
very straightforward to use (note that the macro annotations require
using the [macro-paradise compiler
plugin](https://docs.scala-lang.org/overviews/macros/paradise.html) on
Scala 2.10, 2.11, and 2.12, or `-Ymacro-annotations` on Scala 2.13):

```scala
case class Foo(a: Int, b: Option[String])
object Foo {
  implicit val jCodec = AutomaticJsonCodecBuilder[Foo]
  // alternately: SimpleJsonCodecBuilder[Foo].build("a", _.a, "b", _.b)
}
```

or

```scala
@AutomaticJsonCodec
case class Foo(a: Int, b: Option[String])
```

These can be used to build codecs for any classes which have accessors
that match up to their constructor parameters.  For the `Simple` case,
the names and accessors must be provided in the same order the
constructor takes them, or you will either get an exception when
`build` is called (because it can't find the right constructor) or
you'll get a `JsonCodec` that doesn't roundtrip properly (if the types
of the accessors just happen to line up with the types of the
constructor parameters).  The types of the values to be serialized
must either have `JsonCodec`s themselves, or be `Option`s wrapping
around such types.  `build` comes in variants that will handle up to
22 fields.

The generation for the `Automatic` builders can be affected by placing
annotations (also defined in the `util` package) on the subject
class's constructor's parameters.  The annotations are:

* `@JsonKey("string literal")` which overrides automatic selection of
  the field's name in the generated JSON.
* `@NullForNone` which causes the field to generate a `null` if it is
  an empty `Option`.  Ordinarily empty `Option`s are simply omitted
  from generation altogether.
- `@AllowMissing("scala expression")` which provides a default value
  for a field if it is not present when deserialized.  An `Option` field
  with `@AllowMissing` acts as though it also has `@NullForNone`
* `@LazyCodec` which causes the codec for the field to be resolved
  lazily.  It is ordinarily not necessary, but can be used to stop
  stack overflows or `NullPointerException`s building codecs for
  recursive data structures.
* `@JsonKeyStrategy(strategy)` to override the subject class's default
  key generation strategy.

The two defined strategies are `Strategy.Identity` (the default) and
`Strategy.Underscore` (which converts camel-case names to lower-case,
underscore-separated names).  The `@JsonKeyStrategy` and
`@NullForNone` annotations can also be used on the class level to set
the default strategy for all fields.  If two names map to the same
JSON identifier (whether automatically or through use of `@JsonKey`) a
compiler error occurs.

For classes with type parameters, the annotation macros will build
implicit encoders and/or decoders which require that encoders or
decoders be implicitly available at the point of use:

```scala
@AutomaticJsonCodec
case class Foo[T](things: Seq[T])

// is (approximately) equivalent to

case class Foo[T](things: Seq[T])
object Foo {
  implicit def encode[T: JsonEncode] = AutomaticJsonEncodeBuilder[Foo[T]]
  implicit def decode[T: JsonDecode] = AutomaticJsonDecodeBuilder[Foo[T]]
}
```

For hierarchies of classes, there are analagous `Simple` and
`Automatic` hierarchy codec builders.  The simple builders expect a
chain of branches; the automatic ones derives the chain automatically.
The means by which the different cases are differentiated is
configurable.  The options are `TagToValue` which produces a wrapper
object shaped as `{"typeTag" : "value"}`, `TagAndValue` which produces
a wrapper shaped as `{"tagName" : "typeTag", "valueName" : "value"}`,
`InternalTag` which only works with values that encode as objects and
which adds an additional field to them to serve as the type tag, and
`NoTag` which on decoding simply tries all branches in the order
specified until one succeeds.

Note that the `AutomaticHierarchy` builders can be affected by
[SI-7046](https://issues.scala-lang.org/browse/SI-7046) and
[SI-7588](https://issues.scala-lang.org/browse/SI-7588).

### package com.rojoma.json.v3.interpolation

A string interpolator for building `JValue`s in a syntactically
lightweight way.  The package contains interpolators `json` and `j`;
they are exact synonyms.

```scala
import com.rojoma.json.v3.interpolation._

val x = "world"

// generates { "hello" : "world", "world" : "wide web" }
j"""{ "hello" : $x, $x : "wide web" }"""

// Any encodable can be used in a non-field position
case class A(x: Int)
implicit val aCodec = AutomaticJsonCodecBuilder[A]
j"""{ "a" : ${A(5)} }""" // { "a" : { "x" : 5 } }
```

The interpolators are implemented as macros.  The well-formedness of
the template is checked at compile time.  Optional and spliced
parameters are allowed as well:

```scala
import com.rojoma.json.v3.interpolation._

val a = "one"
val b: Option[String] = Some("two")
val c: Option[String] = None
val d = List("a","b","c")
val e = Map("f" -> 1, "g" -> 2)

/// yields { "a" : "one", "b" : "two", "d" : [1,2,3,"a","b","c"], "f": 1, "g": 2}
j"""{ "a" : $a, b: ?$b, c: ?$c, d: [1, 2, 3, ..$d], ..$e }"""
```

### package com.rojoma.json.v3.conversions.v2

While version 3 of rojoma-json is not binary or source compatible with
v2, it can exist alongside it.  This package contains helpers to
convert between the two versions, to assist in transitions.

The package is meant to have all its contents imported.  It contains
implicit conversions which add `toV3` methods onto rojoma-json-2's
`JsonToken`s, `JsonEvent`s, and `JValue`s and `toV2` methods onto
rojoma-json-3's.  In addition, it can create rojoma-json-2
`JsonCodec`s from instances of 3's `JsonEncode` and `JsonDecode`.

## Cookbook

### Producing JSON from values

To a `Writer`:

```scala
import com.rojoma.json.v3.util.{JsonUtil, ArrayIteratorEncode}
import com.rojoma.json.v3.codec.JsonEncode

def writeIndented[T : JsonEncode](x: T) =
  JsonUtil.writeJson(x, pretty = true)

def writeCompact[T : JsonEncode](x: T) =
  JsonUtil.writeJson(x, pretty = false)

def writeArrayStreaming[T : JsonEncode](x: Iterator[T]) =
  ArrayIteratorEncode.toText(x)
```

To a `String`:

```scala
import com.rojoma.json.v3.util.JsonUtil
import com.rojoma.json.v3.codec.JsonEncode

def formatIndented[T : JsonEncode](x: T) =
  JsonUtil.renderJson(x, pretty = true)

def writeCompact[T : JsonEncode](x: T) =
  JsonUtil.renderJson(x, pretty = false)
```

### Producing values from JSON

From a `Reader`:

```scala
import com.rojoma.json.v3.util.{JsonUtil, JsonArrayIterator}
import com.rojoma.json.v3.codec.{JsonDecode, DecodeError}

def read[T : JsonDecode](r: Reader): Either[DecodeError, T] =
  JsonUtil.readJson[T](r)

// This will throw an `ElementDecodeException` if the JSON is a well-formed
// array but the data cannot be interpreted as a `T`
def readStreaming[T : JsonDecode](r: Reader): Iterator[T] =
  JsonArrayIterator.fromReader[T](r)
```

From a `String`:

```scala
import com.rojoma.json.v3.util.JsonUtil
import com.rojoma.json.v3.codec.{JsonDecode, DecodeError}

def parse[T : JsonDecode](s: String): Either[DecodeError, T] =
  JsonUtil.parseJson[T](s)
```

## Incompatible changes from rojoma-json 2

 * `JNumber` is no longer a case class.
 * `JsonDecode` returns `Either[DecodeError, T]` instead of `Option[T]`.
 * `Pattern#matches` and `Pattern#evaluate` return
   `Either[DecodeError, Pattern.Results]` instead of
   `Option[Pattern.Results]`.
 * `JsonDiff` is gone.  It was cute but useless.
 * `PArray` now requires an exact length match.
 * The position on `JsonToken`s and `JsonEvent`s is now provided in a
   secondary constructor parameter, rather than being a mutable field.
 * The various high-level readers prefer to use block IO instead of
   character-by-character IO.
