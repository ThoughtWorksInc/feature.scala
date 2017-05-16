package com.thoughtworks.feature

import com.thoughtworks.feature.Override.inject
import org.scalatest.{FreeSpec, Matchers}
import shapeless._
import shapeless.labelled.FieldType
import shapeless.record.Record

import scala.annotation.meta.getter

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class OverrideSpec extends FreeSpec with Matchers {
  import OverrideSpec._

  "Override should" - {
    "create class instances" in {
      val o = Override[Record.`'x -> Int, 'y -> Float, 'z -> String`.T, A]
      val a: A = o.newInstance(x = 1, y = 4.5f, z = "z")
      a.x should be(1)
      a.y should be(4.5f)
      a.z should be("z")
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

    "create class instance according to parameter types" in {
      val a: A = Override.newInstance[A](x = 1, y = 4.5f, z = "z")
      a.x should be(1)
      a.y should be(4.5f)
      a.z should be("z")
    }

    "inject dependent type class" in {
      "Override.newInstance[DependentInjection]()" should compile
    }
  }
}

private object OverrideSpec {

  private[OverrideSpec] abstract class A {
    def i: Int = 0
    val x: Int
    def y: Float
    def y(x: Int) = x
    var z: String
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
    val i = 1
    trait LocalApi {
      var i = 1 //Global0.this.i
    }

    type Local <: LocalApi

  }
  trait Global1 {
    trait LocalApi {
      val j = 2
    }
    type Local <: LocalApi
  }

  trait DependentInjection {
    @inject
    val genericPair: Generic[(Int, String)]

    @(inject @getter)
    val `genericPair should be a dependent type`: genericPair.Repr =:= (Int :: String :: HNil)
  }

}
