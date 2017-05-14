package com.thoughtworks.feature

import org.scalatest.{FreeSpec, Matchers}

object CallerSpec {
  object Foo {
    def call(implicit caller: Caller[_]): String = {
      caller.value.getClass.getName
    }
  }

  class IKnowWhatImDoing

  object Foo2{
    def runDangerous()(implicit caller: Caller[IKnowWhatImDoing]) = {
      println(caller.value)
    }
  }

  object Bar2 extends IKnowWhatImDoing {
    Foo2.runDangerous() // ok, prints Bar2
  }

}

final class CallerSpec extends FreeSpec with Matchers {
  import CallerSpec._
  "className" in {
    val className: String = Foo.call
    className should be(this.getClass.getName)
  }

  "restricted" in {
    """
    object Bar {
      Foo2.runDangerous()
    }
    """ shouldNot typeCheck
  }

}
