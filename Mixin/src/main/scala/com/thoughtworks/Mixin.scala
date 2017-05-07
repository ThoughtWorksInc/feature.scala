package com.thoughtworks
import shapeless._

trait Mixin[SuperTypes <: HList] {
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
