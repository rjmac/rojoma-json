package com.rojoma.json.v3
package zipper

import ast._

import org.scalatest.{FunSuite, MustMatchers, EitherValues}

class JsonZipperTests extends FunSuite with MustMatchers with EitherValues {
  def r[T : codec.JsonDecode](s: String) = util.JsonUtil.parseJson[T](s).getOrElse(fail("ack"))

  test("Can descend into an array") {
    JsonZipper(r[JArray]("[1,2,3]")).down_!(1).value must be (JNumber(2))
  }

  test("Can descend into an array and come back up") {
    JsonZipper(r[JArray]("[1,2,3]")).down_!(1).up_!.value must be (r[JArray]("[1,2,3]"))
  }

  test("Descending into an array and coming back up does not allocate a new JValue") {
    val x = r[JArray]("[1,2,3]")
    JsonZipper(x).down_!(1).up_!.value must be theSameInstanceAs (x)
  }

  test("Can descend into an array, change the value, and come back up") {
    JsonZipper(r[JArray]("[1,2,3]")).down_!(1).replace(JString("hello")).up_!.value must be (r[JArray]("[1,'hello',3]"))
  }

  test("Can descend into an array, remove the value, and come back up") {
    JsonZipper(r[JArray]("[1,2,3]")).down_!(1).remove.up_!.value must be (r[JArray]("[1,3]"))
  }

  test("Can descend into an object") {
    JsonZipper(r[JObject]("{hello:1,there:2,world:3}")).down_!("there").value must be (JNumber(2))
  }

  test("Can descend into an object and come back up") {
    JsonZipper(r[JObject]("{hello:1,there:2,world:3}")).down_!("there").up_!.value must be (r[JObject]("{hello:1,there:2,world:3}"))
  }

  test("Descending into an object and coming back up does not allocate a new JValue") {
    val x = r[JObject]("{hello:1,there:2,world:3}")
    JsonZipper(x).down_!("there").up_!.value must be theSameInstanceAs (x)
  }


  test("Can descend into an object, change the value, and come back up") {
    JsonZipper(r[JObject]("{hello:1,there:2,world:3}")).down_!("there").replace(JString("hello")).up_!.value must be (r[JObject]("{hello:1,there:'hello',world:3}"))
  }

  test("Can descend into an object, remove the value, and come back up") {
    JsonZipper(r[JObject]("{hello:1,there:2,world:3}")).down_!("there").remove.up_!.value must be (r[JObject]("{hello:1,world:3}"))
  }

  test("Going up from the top cause a NoSuchElementException") {
    a [NoSuchElementException] must be thrownBy { JsonZipper(JNull).up_! }
  }

  test("Trying to go up from the top must return None") {
    JsonZipper(JNull).up must be (None)
  }

  test("Trying to go to the top from the top must return the original object") {
    val x = r[JArray]("[1,2,3]")
    JsonZipper(x).top.value must be theSameInstanceAs (x)
  }

  test("Trying to go to the top from within an array must return the original object") {
    val x = r[JArray]("[1,2,3]")
    JsonZipper(x).down_!(1).top.value must be theSameInstanceAs (x)
  }

  test("Trying to go to the top from within an object must return the original object") {
    val x = r[JObject]("{hello:1,there:2,world:3}")
    JsonZipper(x).down_!("hello").top.value must be theSameInstanceAs (x)
  }

  test("Trying to go to the top from within a nested array must return the original object") {
    val x = r[JArray]("[1,['a','b','c'],3]")
    JsonZipper(x).down_!(1).asInstanceOf[JArrayZipper].down_!(1).top.value must be theSameInstanceAs (x)
  }

  test("Changing a value from within a nested array must return the modified object") {
    val x = r[JArray]("[1,['a','b','c'],3]")
    JsonZipper(x).down_!(1).asInstanceOf[JArrayZipper].down_!(1).replace(JBoolean(true)).top.value must equal (r[JArray]("[1,['a',true,'c'],3]"))
  }

  test("Changing a value from within a nested object must return the modified object") {
    val x = r[JArray]("[1,{a:true,b:false,c:null},3]")
    JsonZipper(x).down_!(1).asInstanceOf[JObjectZipper].down_!("c").replace(JString("blah")).top.value must equal (r[JArray]("[1,{a:true,b:false,c:'blah'},3]"))
  }


  test("Removing a value from within a nested array must return the modified object") {
    val x = r[JArray]("[1,['a','b','c'],3]")
    JsonZipper(x).down_!(1).asInstanceOf[JArrayZipper].down_!(1).remove.top_!.value must equal (r[JArray]("[1,['a','c'],3]"))
  }

  test("Removing a value from within a nested object must return the modified object") {
    val x = r[JArray]("[1,{a:true,b:false,c:null},3]")
    JsonZipper(x).down_!(1).asInstanceOf[JObjectZipper].down_!("c").remove.top_!.value must equal (r[JArray]("[1,{a:true,b:false},3]"))
  }

  test("Removing the top object and then invoking top_! must throw" ){
    a [NoSuchElementException] must be thrownBy { JsonZipper(JNull).remove.top_! }
  }

  test("Can iterate over the elements of an array") {
    val x = r[JArray]("[1,2,3]")
    JsonZipper(x).down_!(0).value must be (JNumber(1))
    JsonZipper(x).down_!(0).next.get.value must be (JNumber(2))
    JsonZipper(x).down_!(0).next.get.next.get.value must be (JNumber(3))
    JsonZipper(x).down_!(0).next.get.next.get.next must be (None)
  }

  test("Can iterate backwards over the elements of an array") {
    val x = r[JArray]("[1,2,3]")
    JsonZipper(x).down_!(2).value must be (JNumber(3))
    JsonZipper(x).down_!(2).prev.get.value must be (JNumber(2))
    JsonZipper(x).down_!(2).prev.get.prev.get.value must be (JNumber(1))
    JsonZipper(x).down_!(2).prev.get.prev.get.prev must be (None)
  }

  test("Can move from one field to its sibling") {
    val x = r[JObject]("{a:true,b:false,c:null}")
    JsonZipper(x).down_!("b").sibling_!("c").value must equal (JNull)
  }

  test("prev_! and next_! at the toplevel throw NoSuchElementException") {
    a [NoSuchElementException] must be thrownBy { JsonZipper(JNull).prev_! }
    a [NoSuchElementException] must be thrownBy { JsonZipper(JNull).next_! }
  }

  test("sibling_! at the toplevel throws NoSuchElementException") {
    a [NoSuchElementException] must be thrownBy { JsonZipper(JNull).sibling_!("gnu") }
  }

  test("prev_! and next_! in an object throw NoSuchElementException") {
    val z = JsonZipper(r[JObject]("{a:1,b:2,c:3}")).down_!("b")
    a [NoSuchElementException] must be thrownBy { z.prev_! }
    a [NoSuchElementException] must be thrownBy { z.next_! }
  }

  test("sibling_! in an array throws NoSuchElementException") {
    val z = JsonZipper(r[JArray]("[1,2,3]")).down_!(1)
    a [NoSuchElementException] must be thrownBy { z.sibling_!("gnu") }
  }

  test("prev_! and next_! at the ends of arrays throw NoSuchElementException") {
    val z = JsonZipper(r[JArray]("[1,2,3]"))
    val first = z.down_!(0)
    val last = z.down_!(2)

    a [NoSuchElementException] must be thrownBy { first.prev_! }
    a [NoSuchElementException] must be thrownBy { last.next_! }
  }

  test("sibling_! for a nonexistant field throws NoSuchElementException") {
    val z = JsonZipper(r[JObject]("{a:1,b:2,c:3}")).down_!("a")

    a [NoSuchElementException] must be thrownBy { z.sibling_!("gnu") }
  }

  test("down_! out of bounds of an array throws IndexOutOfBoundsException") {
    val z = JsonZipper(r[JArray]("[1,2,3]"))
    an [IndexOutOfBoundsException] must be thrownBy { z.down_!(-1) }
    an [IndexOutOfBoundsException] must be thrownBy { z.down_!(4) }
  }

  test("Remove-and-replace on an array works") {
    JsonZipper(r[JArray]("[1,2,3]")).down_!(1).remove.replace(JString("hello")).top.value must equal (r[JArray]("[1,'hello',3]"))
    JsonZipper(r[JArray]("[1,2,3]")).down_!(1).remove.replace(JString("hello")).next_!.value must equal (JNumber(3))
  }

  test("Remove-and-replace on object works") {
    JsonZipper(r[JObject]("{a:1,b:2,c:3}")).down_!("a").remove.replace(JString("hello")).top.value must equal (r[JObject]("{a:'hello',b:2,c:3}"))
  }
}
