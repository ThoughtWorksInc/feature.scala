package com.thoughtworks.feature

import org.scalatest.{FreeSpec, Matchers}
import shapeless._
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class DemixinSpec extends FreeSpec with Matchers {
  "Demixin" in {
    val demixin = Demixin[String with Int]
    "implicitly[demixin.Out =:= (String :: Int :: HNil)]" should compile
  }
}
