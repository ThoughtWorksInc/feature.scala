package com.thoughtworks.feature

import org.scalatest.{FreeSpec, Matchers}
import scala.language.higherKinds
import The._

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class TheSpec extends FreeSpec with Matchers {

  "Given a method that looking for the implicit value Int <:< Any" - {
    def ask[B <: Int <:< Any](implicit the: The[Int <:< Any, B]): B = {
      the.value
    }
    "When take the narrow value" - {
      def narrow = ask
      "Then it should be an Int <:< Int" in {
        "val singleton: Int <:< Int = narrow" should compile
      }
    }
  }

  "Given a narrow value of Int <:< Any" - {
    def narrow = The[Int <:< Any].value
    "Then it should be an Int <:< Int" in {
      "val singleton: Int <:< Int = narrow" should compile
    }
  }
}
