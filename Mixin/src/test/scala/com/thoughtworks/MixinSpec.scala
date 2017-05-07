package com.thoughtworks

import com.thoughtworks.MixinSpec._
import org.scalatest.{FreeSpec, Matchers}
import shapeless._
import shapeless.ops.hlist.Comapped
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class MixinSpec extends FreeSpec with Matchers {
  "Comapped then Mixin" in {
    val setting1 = new Setting1
    val setting2 = new Setting2
    val setting3 = new Setting3

    type TypeExtractor[A] = { type InnerTrait = A }
    val comapped = Comapped[setting1.type :: setting2.type :: setting3.type :: HNil, TypeExtractor]
    val mixin = Mixin[comapped.Out]
    "implicitly[mixin.Out =:= (setting1.InnerTrait with setting2.InnerTrait with setting3.InnerTrait)]" should compile
  }
}
object MixinSpec {
  implicit def hlistComapped[FH, FT <: HList, F[_], H, TCM <: HList](
      implicit mt: Comapped.Aux[FT, F, TCM],
      constraint: FH <:< F[H]): Comapped.Aux[FH :: FT, F, H :: TCM] =
    new Comapped[FH :: FT, F] { type Out = H :: TCM }

  final class Setting1 {
    trait InnerTrait
  }

  final class Setting2 {
    trait InnerTrait
  }
  final class Setting3 {
    trait InnerTrait
  }

}
