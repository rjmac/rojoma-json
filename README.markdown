# rojoma-json

## Getting it

There is a maven-ish repository at http://rjmac.github.com/maven/ --
setting up SBT is as simple as

```scala
resolvers += "rojoma.com" at "http://rjmac.github.com/maven/releases/"

libraryDependencies += "com.rojoma" %% "rojoma-json" % "1.3.23"
```

While for Maven, the pom snippets are:

```xml
<repositories>
  <repository>
    <id>rojoma.com</id>
    <url>http://rjmac.github.com/maven/releases/</url>
  </repository>
</repositories>

<dependencies>
  <dependency>
    <groupId>com.rojoma</groupId>
    <artifactId>rojoma-json_${scala.version}</artifactId>
    <version>1.3.23</version>
  </dependency>
</dependencies>
```

## Documentation

### package com.rojoma.json.ast

 * `JValue`: An AST for JSON
    * `JAtom`
       * `JNull`
       * `JBoolean(boolean: Boolean)`
       * `JString(string: String)`
       * `JNumber(bigdecimal: BigDecimal)`
    * `JCompound`
       * `JArray(toSeq: scala.collection.Seq[JValue])`
       * `JObject(fields: scala.collection.Map[String, JValue])`

The `JCompound` classes extend `Iterable` and have convenience methods
that make them act like `Seq` and `Map` respectively, but are not
themselves actually `Seq`s or `Map`s.  Use the `toSeq`, or `fields` or
`toMap`, method to get a real one.

All `JValue`s have a `cast[T]` method that can be used to safely
downcast to a more specific type.

### package com.rojoma.json.codec

 * `JsonCodec[T]`: a typeclass for converting objects to/from JSON
    * `encode(x: T): JValue`
    * `decode(x: JValue): Option[T]`

The following types have implicit codecs in `JsonCodec`'s compantion:

 * `String`
 * `Boolean`
 * Numeric types, including `BigInt`, `BigDecimal`, and their `java.math` counterparts
 * `JValue` and all its subclasses
 * Any `S[T] <: Seq[T]` if `T` has a `JsonCodec` and `S` has an implicit `CanBuild`
 * Any `M[String, T] <: Map[String, T]` if `T` has a `JsonCodec` and `M` has an implicit `CanBuild`
 * `java.util.List[T]` if `T` has a `JsonCodec`
 * `java.util.Map[String, T]` if `T` has a `JsonCodec`

Numeric codecs are "lenient" -- that is, if a number is out of range
of the requested type, it undergoes the normal truncation
`BigDecimal.toXXX` does.  If this is not desired, request a
`BigDecimal` or a `JValue` and use the `.toXXXExact` alternatives.

### package com.rojoma.json.io

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

The only limits on the sizes of strings and depth of nesting are
those of the JVM.  Parsing is done recursively, and so stack space
is the limiting factor in nesting.  Numbers are restricted to
those which can fit in a Java `BigDecimal`.  The reader does validate
surrogate pairs and will replace stray halves with the Unicode
`REPLACEMENT_CHARACTER` character.

### package com.rojoma.json.matcher

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

Most `Pattern`s can also be used to generate JSON using the `generate`
method, passing in a list of variable bindings in the form `variable := value` or,
if the variable occurs in an "optional" position (i.e., inside a `POption`
or `FirstOf`) `variable :=? optValue`, where `optValue` is an `Option`.

### package com.rojoma.json.zipper

A zipper for navigating JSON.  There are six interfaces:

 * `JsonZipper`
    * `JAtomZipper`
    * `JCompoundZipper`
       * `JArrayZipper`
       * `JObjectZipper`
 * `NothingZipper`

Each one is parameterized with the type of its "parent" zipper in the
path from the root of the JSON object being traversed.  An array
or object zipper may be acquired by calling `asArray` or `asObject`
on a generic zipper.

Each of the first five allows you to move `up`, to the `top` of the
object, or find the object `here` or `replace` it.  In addition, the
array and object zippers allow `replace`ing or `remove`ing child
elements.

The `NothingZipper` is special -- it is what is returned from removing
the current object.  With a `NothingZipper` you can either put a new
object in the hole it represents (via the `replace` method) or move
`up` or to the `top`.  Unlike the `JsonZipper` classes, when you have
nothing `top` might not return anything, since it is possible that the
root object is what was removed.

### package com.rojoma.json.jpath

The `JPath` class is a simple wrapper over `JsonZipper`s for doing
"xpath-style" queries on a `JValue`.

Conceptually, a `JPath` object is a collection of pointers into a JSON
object, on which operations (methods on the object) are performed in
parallel, producing a new `JPath`.  If an operation cannot be
performed on one of the pointers, it simply does not contribute to the
new object.  The most important methods are:

* `down(String)` : descend into the named field of an object.
* `down(Int)` : descend into the specified index of an array.
* `*` : in parallel, go down into all elements of an object or array.
* `rec` : add every subobject to the collection recursively, including the current node.
* `**` : add every subobject to the collection recursively, excluding the current node.
* `where(JsonZipper[_] => Boolean)` : filter the collection to those nodes for which the predicate is true.
* `having(JPath => JPath)` : filter the collection to those nodes for which the function returns a non-empty collection.
* `up` : move up to the collection's nodes' parents.  Note: this does not re-combine identical nodes; `*.up` will have the original node(s) duplicated once for each of their children.

In addition, there are a number of operations which I have found to be
less useful in practice, but which still exist: `downLast` and
`downFirst` (which act on arrays and objects); and `next` and `prev`
which act on child-nodes.

When done chaining operations, call `finish`, which turns the `JPath`
object into a stream of `JValue`s.

### package com.rojoma.json.util

Utility operations that combine parts from other packages.  The main
member of this package is the object `JsonUtil` which contains
convenience methods for moving data all the way between character data
and usable objects. 

However, despite my distrust of all things reflective, this package
also contains an object called `SimpleJsonCodecBuilder`, which uses a
tiny amount of reflection to make producing `JsonCodec`s for case
classes easier.  It's very straightforward to use:

```scala
case class Foo(a: Int, b: Option[String])
object Foo {
  implicit val jCodec = SimpleJsonCodecBuilder[Foo].gen("a", _.a, "b", _.b)
}
```

This can be used to build codecs for any classes which have accessors
that match up to their constructor parameters.  The names and
accessors must be provided in the same order the constructor takes
them, or you will either get an exception when `gen` is called
(because it can't find the right constructor) or you'll get a
`JsonCodec` that doesn't roundtrip properly (if the types of the
accessors just happen to line up with the types of the constructor
parameters).  The types of the values to be serialized must either
have `JsonCodec`s themselves, or be `Option`s wrappring around such
types.  `gen` comes in variants that will handle up to 22 fields.
