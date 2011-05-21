package com.rojoma.json
package io

import ast._

sealed abstract class JsonXException(msg: String) extends Exception(msg)
case class InvalidRootElementException() extends JsonXException("Root element must be an object or array")

object JsonX {
  @throws(classOf[JsonXException])
  def toXml(x: JValue): xml.Elem = x match {
    case obj: JObject => schemaize(innerToXml(obj))
    case arr: JArray => schemaize(innerToXml(arr))
    case _ => throw InvalidRootElementException()
  }

  private def schemaize(e: xml.Elem): xml.Elem = {
    def att(ns: String, name: String, value: String) = new xml.PrefixedAttribute(ns, name, value, xml.Null)
    e % att("xsi", "schemaLocation", "http://www.datapower.com/schemas/json jsonx.xsd") % att("xmlns", "xsi", "http://www.w3.org/2001/XMLSchema-instance") % att("xmlns", "json", "http://www.ibm.com/xmlns/prod/2009/jsonx")
  }

  private def innerToXml(x: JValue): xml.Elem = x match {
    case JNull =>
      <json:null/>
    case JIntegral(x) =>
      <json:number>{x}</json:number>
    case JFloatingPoint(x) =>
      <json:number>{x}</json:number>
    case JString(x) =>
      <json:string>{x}</json:string>
    case JBoolean(x) =>
      <json:boolean>{x}</json:boolean>
    case JArray(xs) =>
      <json:array>{xs map innerToXml}</json:array>
    case JObject(fields) =>
      <json:object>{fields map { case (name, value) =>
        innerToXml(value) % new xml.UnprefixedAttribute("name", name, xml.Null)
      }}</json:object>
  }
}
