package com.thoughtworks

import com.thoughtworks.Override.{inject, mixinLowerBounds}
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

  "xx" in {
    Override[HNil, AbstractTypeOwner0 with AbstractTypeOwner1]
    Override[HNil, Global0 with Global1]
    new Global0 with Global1 {
      override type Local = super[Global0].LocalApi with super[Global1].LocalApi
      override def xxx = implicitly
    }
  }

//  "yy" in {
//    Override[HNil, Injected]
//  }

}

private object OverrideSpec {

  private[OverrideSpec] abstract class A {
    def i: Int = 0
    def x: String
    def y: String
  }

  trait AbstractTypeOwner0 {
    type T >: List[Nothing] <: Seq[Any]
  }

  trait AbstractTypeOwner1 {
    type T <: Iterable[String]
  }

  trait Injected {
    @inject
    def foo(implicit x: Ordering[Int]): Int =:= Int
  }

  trait Global0 {
    trait LocalApi

    type Local <: LocalApi

    @inject
    def xxx: Override[HNil, Local]

  }
  trait Global1 {
    trait LocalApi
    type Local <: LocalApi

  }

}
