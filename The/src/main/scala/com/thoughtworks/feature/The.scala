package com.thoughtworks.feature
import scala.language.higherKinds

final class The[Widen, Narrow] private[The] (val value: Narrow) extends AnyVal

object The {
  implicit def apply[Widen](implicit t: Widen): The[Widen, t.type] = new The[Widen, t.type](t)
}
