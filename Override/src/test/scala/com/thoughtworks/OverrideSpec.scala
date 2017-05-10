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

  "Override should" - {
    "create class instances" in {
      val o = Override[Record.`'x -> String, 'y -> String`.T, A]
      val a: A = o.newInstance(x = "x", y = "y")
      a.x should be("x")
      a.y should be("y")
    }

    "merge abstract types" in {
      val o = Override[HNil, AbstractTypeOwner0 with AbstractTypeOwner1]
      val mixed = o.newInstance()
      implicitly[mixed.T <:< Iterable[Any]]
    }

    "create the implementation for @inject method from implicit values" in {
      Override[HNil, Injected]
    }

    "create the implementation for @inject method with an abstract return type from implicit values" in {
      val o = Override[HNil, Global0 with Global1 with HasLocal]
      val g = o.newInstance()
      val l = g.local
      val local = l.newInstance()
      (local: Global0#Local).i should be(1)
      (local: Global1#Local).j should be(2)
    }
  }
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
  trait HasLocal {
    type Local
    @inject
    def local: Override[HNil, Local]

  }
  trait Global0 {
    trait LocalApi {
      val i = 1
    }

    type Local <: LocalApi

  }
  trait Global1 {
    trait LocalApi {
      val j = 2
    }
    type Local <: LocalApi

  }

}
