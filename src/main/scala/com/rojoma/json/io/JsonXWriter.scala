package com.rojoma.json
package io

import ast._

// For the random arbitrary limits on sizes JSONx specifies
sealed abstract class JsonXException(msg: String) extends Exception(msg)
case class StringTooLongException() extends JsonXException("Maximum string-length exceeded")
case class MaximumNestingDepthExceeded() extends JsonXException("Maximum nesting depth exeeded")

object JsonX {
  @throws(classOf[JsonXException])
  def toXml(x: JCompound): xml.Elem = x match {
    case obj: JObject => schemaize(innerToXml(obj, 0))
    case arr: JArray => schemaize(innerToXml(arr, 0))
  }

  val MaximumStringLength = 8192
  val MaximumNestingDepth = 64
  // max length for Numbers is 128; since we use Longs & Doubles, we
  // can't reach that.

  private def schemaize(e: xml.Elem): xml.Elem = {
    def att(ns: String, name: String, value: String) = new xml.PrefixedAttribute(ns, name, value, xml.Null)
    e % att("xsi", "schemaLocation", "http://www.datapower.com/schemas/json jsonx.xsd") % att("xmlns", "xsi", "http://www.w3.org/2001/XMLSchema-instance") % att("xmlns", "json", "http://www.ibm.com/xmlns/prod/2009/jsonx")
  }

  private def innerToXml(x: JValue, depth: Int): xml.Elem = {
    if(depth > MaximumNestingDepth) throw MaximumNestingDepthExceeded()
    x match {
      case JNull =>
        <json:null/>
      case JIntegral(x) =>
        <json:number>{x}</json:number>
      case JFloatingPoint(x) =>
        if(x.isNaN || x.isInfinite)
          throw JsonInvalidFloat(x)
        <json:number>{x}</json:number>
      case JString(x) =>
        if(x.length > MaximumStringLength) throw StringTooLongException()
        <json:string>{x}</json:string>
      case JBoolean(x) =>
        <json:boolean>{x}</json:boolean>
      case JArray(xs) =>
        <json:array>{xs map (innerToXml(_, depth+1))}</json:array>
      case JObject(fields) =>
        <json:object>{fields map { case (name, value) =>
          innerToXml(value, depth+1) % new xml.UnprefixedAttribute("name", name, xml.Null)
        }}</json:object>
    }
  }
}
