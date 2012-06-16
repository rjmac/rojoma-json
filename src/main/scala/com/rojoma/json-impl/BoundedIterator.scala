package com.rojoma.`json-impl`

class BoundedIterator[T](start: T, middle: Iterator[T], end: T) extends Iterator[T] {
  private[this] var state = 0 // 0 = before start, 1 = processing middle or end, 2 = after end
  def hasNext = state != 2
  def next() = state match {
    case 0 =>
      state = 1
      start
    case 1 =>
      if(middle.hasNext) {
        middle.next()
      } else {
        state = 2
        end
      }
    case 2 =>
      Iterator.empty.next()
  }
}
