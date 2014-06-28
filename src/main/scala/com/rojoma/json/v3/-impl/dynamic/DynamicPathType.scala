package com.rojoma.json.v3
package `-impl`.dynamic

sealed abstract class DynamicPathType[T](val isField: Boolean) {
  def asField(subFieldOrIndex: T): String
  def asIndex(subFieldOrIndex: T): Int
}

object DynamicPathType {
  implicit object str extends DynamicPathType[String](isField = true) {
    def asField(subField: String) = subField
    def asIndex(subField: String) = throw new IllegalStateException("asIndex on a non-index")
  }

  implicit object int extends DynamicPathType[Int](isField = false) {
    def asField(subIndex: Int) = throw new IllegalStateException("asField on a non-field")
    def asIndex(subIndex: Int) = subIndex
  }
}
