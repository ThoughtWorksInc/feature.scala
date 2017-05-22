package com.thoughtworks.feature
import shapeless._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait Demixin[ConjunctionType] {
  type Out <: HList
}

object Demixin {

  type Aux[ConjunctionType, Out0] = Demixin[ConjunctionType] {
    type Out = Out0
  }

  def apply[ConjunctionType](implicit demixin: Demixin[ConjunctionType])
    : Demixin.Aux[ConjunctionType, demixin.Out] =
    demixin

  implicit def materialize[ConjunctionType]: Demixin[ConjunctionType] =
    macro Macros.materialize[ConjunctionType]

  final class Macros(val c: whitebox.Context) extends CaseClassMacros {
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
