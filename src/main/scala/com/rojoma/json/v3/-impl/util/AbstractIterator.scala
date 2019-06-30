package com.rojoma.json.v3
package `-impl`.util

import scala.collection.BufferedIterator

abstract class AbstractIterator[+A] extends Iterator[A]
abstract class AbstractBufferedIterator[+A] extends AbstractIterator[A] with BufferedIterator[A]
