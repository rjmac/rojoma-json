3.13.0 (unreleased)
===================
* JObject and JArray have "empty" in addition to (and preferred to) `canonicalEmpty`
* On scala 2.12, `JBoolean.apply` now alawys returns `canonicalTrue` or `canonicalFalse`
* On scala 2.12, `JObject` and `JArray.apply` return `empty` if given an empty collection
* Small integer `JNumber`s are now interned, if constructed with an integral type.

3.12.0
======
* Add `@ForbidUnknownFields` annotation, used by the automatic decode builder

3.11.0
======
* Optional codecs for `java.time` classes

3.10.1
======
* Internal improvements to the json interpolator
* Canonical empty arrays and objects force to themselves

3.10.0
======
* Add `?${...}` syntax to the interpolator for optional interpolations
* Add `..${...}` syntax to the interpolator for splicing interpolations
* Make `@AutomaticJsonCodec` produce separate encode and decode values
* Make `@AutomaticJsonCodec` and friends work with classes that have type parameters

3.9.3
=====
* Improve modifiers generated on companion objects created by `@AutomaticJsonCodec` and friends.

3.9.1
=====
* Fix BigDecimal limit detection

3.9.0
=====
* Make enum codecs optionally case-insensitive

3.8.0
=====
* Provide codecs for Java boxed-primitive types

3.7.2
=====
* Add FixedSetFieldCache
* Publish for 2.12

3.7.0
=====
* Allow using `@NullForNone` on the class level
* Fix JString's `FieldEncode` instance
* Allow customization of string serialization via the extension point system
* Add `@AlternativeJsonKey` annotation
* Add `adjust` to `JsonZipper`

3.6.0
=====
* Add the ability to get a `JsonZipper` out of a dynamic `JValue`

3.5.0
=====
* Add a bunch of `writeJsonFile` overloads
* Make `DecodeError` a `RuntimeException`

3.4.0
=====
* Add support for generating codecs for Scala enumerations

3.3.0
=====
* Add the notion of extension points, and allow customization of number serialization with it.
* Decode errors prefer the longest path
* Allow `ArrayIteratorEncode` to produce tokens and events as well as text

3.2.1
=====
* Fix that `JsonParserEOF` couldn't be roundtripped through JSON.

3.2.0
=====
* Introduce field codecs for allowing serializing maps with non-String keys

3.1.2
=====
* Fix that malformed interpolations would throw NotImplementedException in Scala 2.10

3.1.1
=====
* Improve dynamic jvalues

3.1.0
=====
* Less verbose WrapperJsonCodecs
