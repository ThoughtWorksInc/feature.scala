package com.thoughtworks.feature

import scala.reflect.macros.whitebox
import scala.language.experimental.macros

/** A type class that extracts the self-type of `A`.
  *
  * @example Given a trait without an explicit self-type declaration
  *
  *          {{{
  *          trait MyTrait
  *          }}}
  *
  *          Then the self type should be itself
  *
  *          {{{
  *          val selfType = SelfType[MyTrait]
  *          "implicitly[selfType.Out =:= MyTrait]" should compile
  *          }}}
  *
  * @example Given a trait with an explicit self-type declaration
  *
  *          {{{
  *          trait MySelfType
  *          trait MyTrait { this: MySelfType => }
  *          }}}
  *
  *          Then the self type should be itself
  *
  *          {{{
  *          val selfType = SelfType[MyTrait]
  *          "implicitly[selfType.Out =:= (MySelfType with MyTrait)]" should compile
  *          }}}
  *
  * @example Given a compound type
  *
  *          {{{
  *          trait MySelfType1
  *          trait MyTrait1 { this: MySelfType1 => }
  *
  *          trait MyTrait2
  *
  *          trait MySelfType3
  *          trait MyTrait3 { this: MySelfType3 => }
  *
  *          type MyCompoundType = MyTrait1 with MyTrait2 with MyTrait3 {
  *            type MyRefinement = Int
  *          }
  *          }}}
  *
  *          Then the self type should be a compound type of each mix-in part
  *
  *          {{{
  *          val selfType = SelfType[MyCompoundType]
  *          "implicitly[selfType.Out =:= (MySelfType1 with MySelfType3 with MyTrait1 with MyTrait2 with MyTrait3)]" should compile
  *          }}}
  *
  *          And the self type should not contain the refinement statements
  *
  *          {{{
  *          "implicitly[selfType.Out <:< { type MyRefinement = Int }]" shouldNot typeCheck
  *          }}}
  *
  */
trait SelfType[A] {
  type Out
}

object SelfType {

//  final class SelfTypeAux[A, Out0] extends SelfType[A] {
//    type Out = Out0
//  }

  type Aux[A, Out0] = SelfType[A] {
    type Out = Out0
  }

  def make[A, Out0]: SelfType.Aux[A, Out0] = new SelfType[A] {
    type Out = Out0
  }

  implicit def apply[A]: SelfType[A] = macro Macros.apply[A]

  private[SelfType] final class Macros(val c: whitebox.Context) {
    import c.universe._

    def apply[A: WeakTypeTag]: Tree = {
      val a = weakTypeOf[A]
      val selfTypes: List[Type] = {
        val selfTypeBuilder = List.newBuilder[Type]
        def buildSelfTypes(t: Type): Unit = {
          val dealiased = t.dealias
          dealiased match {
            case RefinedType(superTypes, refinedScope) =>
              superTypes.foreach(buildSelfTypes)
            case typeRef: TypeRef =>
              val symbol = dealiased.typeSymbol
              if (symbol.isClass) {
                selfTypeBuilder += symbol.asClass.selfType.asSeenFrom(dealiased, symbol)
              }
            case _ =>
          }
        }
        buildSelfTypes(a)
        selfTypeBuilder.result()
      }
      val out = selfTypes match {
        case Nil =>
          definitions.AnyTpe
        case _ =>
          internal.refinedType(selfTypes, c.internal.enclosingOwner)
      }
      q"_root_.com.thoughtworks.feature.SelfType.make[$a, $out]"

    }

  }

}
