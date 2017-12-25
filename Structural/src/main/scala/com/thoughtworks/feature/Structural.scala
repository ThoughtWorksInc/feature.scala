package com.thoughtworks.feature

import simulacrum.{op, typeclass}

import scala.reflect.macros.whitebox
import scala.language.experimental.macros
import scala.language.implicitConversions
import com.thoughtworks.Extractor._

import scala.reflect.ClassTag

/** A type class that converts a class type to a refinement type
  *
  * This [[Structural]] type class provides an extension method [[structuralize]] via implicit view,
  * which can be import as follow:
  * {{{
  * import com.thoughtworks.feature.Structural.ops._
  * }}}
  *
  * @example Given a String
  *
  *          {{{
  *          val myString: String = "foo"
  *          }}}
  *
  *          When converting it to a structural type
  *          {{{
  *          val myStruct = myString.structuralize
  *          }}}
  *
  *          Then methods on the converted struct should be able to access via reflective call
  *
  *          {{{
  *          import scala.language.reflectiveCalls
  *          myStruct.startsWith("f") should be(true)
  *          }}}
  *
  *          However, the struct is not a String
  *
  *          {{{
  *          "struct: String" shouldNot typeCheck
  *          }}}
  *
  * @example Given a class that contains abstract types:
  *
  *          {{{
  *          trait Container extends Iterable[String] {
  *            type Xxx >: Iterable[String]
  *            def self: Xxx = this
  *          }
  *          val container: Container = new Container { def iterator = Iterator.empty }
  *          }}}
  *
  *          When converting it to a structural type
  *
  *          {{{
  *          val myStruct = container.structuralize
  *          }}}
  *
  *          Then methods on the converted struct should be able to access via reflective call
  *
  *          {{{
  *          myStruct.self should be(myStruct)
  *          }}}
  *
  */
@typeclass
trait Structural[Mixin] {
  type Out >: Mixin

  final def structuralize(mixin: Mixin): Out = mixin
}

object Structural {

  type Aux[Mixin, Out0] = Structural[Mixin] {
    type Out = Out0
  }

  /** For internal use only!
    */
  val typeClassCache = new Structural[Any] {
    type Out = Any
  }

  implicit def materialize[Mixin]: Structural[Mixin] = macro Macros.apply[Mixin]

  private[Structural] final class Macros(val c: whitebox.Context) {

    import c.universe._
    import definitions._

    def apply[Mixin: WeakTypeTag]: Tree = {
      val mixin = weakTypeOf[Mixin]
      val mixinSymbol = mixin.typeSymbol
      val untyper = new Untyper[c.universe.type](c.universe) {
        private def replaceThisValue: PartialFunction[Type, Tree] = {
          case tt @ ThisType(symbol) if symbol == mixinSymbol =>
            q"this"
        }
        override def singletonValue: PartialFunction[Type, Tree] = {
          replaceThisValue.orElse(super.singletonValue)
        }
      }
      val mixinThis = internal.thisType(mixinSymbol)

      val scopeBuilder = List.newBuilder[Scope]
      val superTypeListBuilder = List.newBuilder[Type]
      val anyTypeList = definitions.AnyTpe :: Nil

      def buildDuckList(t: Type): Unit = {
        val dealiased = t.dealias
        dealiased match {
          case RefinedType(superTypes, refinedScope) =>
            superTypes.foreach(buildDuckList)
            scopeBuilder += refinedScope
          case _ =>
            superTypeListBuilder += dealiased
            scopeBuilder += dealiased.members
        }
      }

      buildDuckList(mixin)
      val superTypes: List[Type] = superTypeListBuilder.result()
      val scopes = scopeBuilder.result()
      val out = scopes
        .map { scope =>
          val refinements = for {
            symbol <- scope
            if symbol.isPublic && !symbol.isConstructor && (symbol.owner match {
              case owner if owner == AnyClass || owner == AnyRefClass || owner == ObjectClass =>
                false
              case _ =>
                true
            }) && !symbol.isClass
          } yield {
            untyper.definition(mixinThis)(symbol)
          }

          tq"Any { ..$refinements }"
        }
        .reduce { (a, b) =>
          tq"$a with $b"
        }
      q"""_root_.com.thoughtworks.feature.Structural.typeClassCache.asInstanceOf[
        _root_.com.thoughtworks.feature.Structural.Aux[$mixin, $out]
      ]"""

    }

  }

}
