package com.rojoma.`json-impl`

abstract class AbstractIterator[+A] extends Iterator[A]
abstract class AbstractBufferedIterator[+A] extends AbstractIterator[A] with BufferedIterator[A]
