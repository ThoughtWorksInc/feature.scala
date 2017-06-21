package com.thoughtworks.feature
import scala.language.higherKinds

/** A helper that summons dependent type class `Widen`.
  *
  * @note `The` can be used as an alternative to [[scala.Predef.implicitly implicitly]] for dependent type classes.
  *       {{{
  *       val narrowed: Int <:< Int = The[Int <:< Any].value
  *       }}}
  * @see [[https://github.com/milessabin/shapeless/pull/695]] for discussion about the motivation of `The`.
  */
final class The[Widen, Narrow <: Widen] private[The] (val value: Narrow) extends AnyVal

object The {
  implicit def apply[Widen](implicit value: Widen): The[Widen, value.type] = new The[Widen, value.type](value)
}
