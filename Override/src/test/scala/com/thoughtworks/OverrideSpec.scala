package com.thoughtworks

import org.scalatest.{FreeSpec, Matchers}
import shapeless._
import shapeless.labelled.FieldType
import shapeless.record.Record

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class OverrideSpec extends FreeSpec with Matchers {
  import OverrideSpec._

  "Override should create class instances" in {
    val o = Override[Record.`'x -> String, 'y -> String`.T, A]
    val a: A = o.newInstance(x = "x", y = "y")
    a.x should be("x")
    a.y should be("y")
  }
}

private object OverrideSpec {

  private[OverrideSpec] abstract class A {
    def i: Int = 0
    def x: String
    def y: String
  }

}
