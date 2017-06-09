package com.thoughtworks.feature
import shapeless._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/** A type class that converts a mix-in type to [[shapeless.HList]].
  *
  *
  * == Common imports ==
  *
  * You may want to use [[Demixin]] with [[shapeless.HList]].
  *
  * {{{
  * import shapeless._
  * import org.scalatest.Matchers._
  * }}}
  *
  * @example [[Out]] of [[Demixin]] on non-mixed-in types other than [[scala.Any]] should be a [[shapeless.HList]] that contains only one element
  *
  *          {{{
  *          val demixin = Demixin[String]
  *          "implicitly[demixin.Out =:= (String :: HNil)]" should compile
  *          }}}
  *
  * @example [[Out]] of [[Demixin]] on [[scala.Any]] should be [[shapeless.HNil]]
  *
  *          {{{
  *          val demixin = Demixin[Any]
  *          "implicitly[demixin.Out =:= HNil]" should compile
  *          }}}
  *
  * @example The [[Demixin]] type class can be summoned from [[Demixin.apply]] method:
  *
  *          {{{
  *          class A; trait B; object C;
  *          val demixin = Demixin[A with B with C.type with String with Int]
  *          }}}
  *
  *          [[Out]] should be a [[shapeless.HList]] of each type components in the mix-in type `ConjunctionType`.
  *
  *          {{{
  *          "implicitly[demixin.Out =:= (A :: B :: C.type :: String :: Int :: HNil)]" should compile
  *          }}}
  *
  *          The elements in [[Out]] should keep the same order as type components in `ConjunctionType`.
  *
  *          {{{
  *          "implicitly[demixin.Out =:!= (String :: A :: B :: C.type :: Int :: HNil)]" should compile
  *          }}}
  */
trait Demixin[ConjunctionType] {
  type Out <: HList
}

object Demixin {

  type Aux[ConjunctionType, Out0] = Demixin[ConjunctionType] {
    type Out = Out0
  }

  def apply[ConjunctionType](implicit demixin: Demixin[ConjunctionType]): Demixin.Aux[ConjunctionType, demixin.Out] =
    demixin

  implicit def materialize[ConjunctionType]: Demixin[ConjunctionType] =
    macro Macros.materialize[ConjunctionType]

  private[Demixin] final class Macros(val c: whitebox.Context) extends CaseClassMacros {
    import c.universe._

    private def demixin(t: Type): Stream[Type] = {
      t.dealias match {
        case RefinedType(superTypes, refinedScope) if refinedScope.isEmpty =>
          superTypes.toStream.flatMap(demixin)
        case any if definitions.AnyTpe <:< any =>
          Stream.empty[Type]
        case notRefinedType =>
          Stream(notRefinedType)
      }
    }

    def materialize[ConjunctionType: WeakTypeTag]: Tree = {
      val conjunctionType = weakTypeOf[ConjunctionType]
      val out = mkHListTpe(demixin(conjunctionType).distinct.toList)
      val result = q"""
        new _root_.com.thoughtworks.feature.Demixin[$conjunctionType] {
          type Out = $out
        } : _root_.com.thoughtworks.feature.Demixin.Aux[$conjunctionType, $out]
      """
//      c.info(c.enclosingPosition, showCode(result), true)
      result
    }
  }

}
