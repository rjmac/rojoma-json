package com.rojoma.`json-impl`

// Workaround for Iterator#flatten being insufficiently lazy in 2.9.0

class FlatteningIterator[T](its: Iterator[Iterator[T]]) extends Iterator[T] {
  private var current: Iterator[T] = Iterator.empty

  def hasNext = {
    while(!current.hasNext && its.hasNext) current = its.next()
    current.hasNext
  }

  def next() = {
    hasNext // ensure we're either on a non-empty iterator or at the final end of input
    current.next()
  }
}
