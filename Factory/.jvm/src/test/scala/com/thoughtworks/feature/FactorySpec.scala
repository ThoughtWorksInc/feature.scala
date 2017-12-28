package com.thoughtworks.feature

import com.thoughtworks.feature.Factory.inject
import org.scalatest.{FreeSpec, Matchers}
import shapeless.Witness

/**
  * @author 杨博 (Yang Bo)
  */
class FactorySpec extends FreeSpec with Matchers {

  "It should automatically include self-types" in {
    trait A {
      def intValue: Int
      @inject
      val intOrdering: Ordering[Int]
    }
    trait B { this: A =>
      val stringValue: String
      @inject
      def floatOrdering: Ordering[Float]
    }

    val ab = Factory[B with B with Any].newInstance(intValue = 42, stringValue = "foo")
    ab should be(a[A])
    ab should be(a[B])
  }

  "It should not error if self-type is an existential type" in {
    trait A[T]
    trait B { this: A[_] =>
    }

    val ab = Factory[A[String] with B].newInstance()
    ab should be(a[A[_]])
    ab should be(a[B])
  }

  "@inject should support shapeless.Witness" in {
    trait A {
      @inject
      def witness42: Witness.Aux[Witness.`42`.T]
    }
    Factory[A].newInstance().witness42.value should be(42)
  }

}
