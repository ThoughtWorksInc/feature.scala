package com.thoughtworks.feature
import shapeless._

/** A type class that converts a [[shapeless.HList]] to a mix-in type.
  *
  *
  * == Common imports ==
  *
  * You may want to use [[Mixin]] with [[shapeless.HList]].
  *
  * {{{
  * import shapeless._
  * }}}
  *
  * @example [[Out]] of [[Mixin]] is a minx-in type that consists of each types in `L`
  *
  *          {{{
  *          val mixin = Mixin[Int :: Nil.type :: String :: HNil]
  *          "implicitly[mixin.Out =:= (Int with Nil.type with String)]" should compile
  *          }}}
  */
trait Mixin[L <: HList] {
  type Out
}

private[thoughtworks] trait LowPriorityMixin { this: Mixin.type =>
  implicit def hconsMixin[Head, TailOut, Tail <: HList](
      implicit tailMixin: Mixin.Aux[Tail, TailOut]): Mixin.Aux[Head :: Tail, Head with TailOut] =
    new Mixin[Head :: Tail] {
      override type Out = Head with TailOut
    }
}

object Mixin extends LowPriorityMixin {
  type Aux[SuperTypes <: HList, Out0] = Mixin[SuperTypes] {
    type Out = Out0
  }
  def apply[SuperTypes <: HList](implicit mixin: Mixin[SuperTypes]): Mixin.Aux[SuperTypes, mixin.Out] = mixin
  implicit def singletonMixin[Head]: Mixin.Aux[Head :: HNil, Head] = new Mixin[Head :: HNil] {
    override type Out = Head
  }
}
