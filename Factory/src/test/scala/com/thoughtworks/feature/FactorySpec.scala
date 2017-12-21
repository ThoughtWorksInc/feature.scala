package com.thoughtworks.feature

import org.scalatest.{FreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo)
  */
class FactorySpec extends FreeSpec with Matchers {

  "It should automatically include self-types" in {
    trait A
    trait B { this: A =>
    }

    val ab = Factory[B].newInstance()
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

}

object FactorySpec {}
