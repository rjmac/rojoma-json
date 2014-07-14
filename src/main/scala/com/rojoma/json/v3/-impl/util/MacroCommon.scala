package com.rojoma.json.v3
package `-impl`.util

trait MacroCommon { this: MacroCompat =>
  import c.universe._

  protected def isType(t: Type, w: Type) = {
    // There HAS to be a better way to do this.
    // t MAY be <error>.  w must not be!
    // since <error> =:= any type, reject if it looks "impossible".
    t =:= w && !(t =:= typeOf[String] && t =:= typeOf[Map[_,_]])
  }
}
