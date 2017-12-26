package com.thoughtworks.feature

import simulacrum.{op, typeclass}

import scala.reflect.macros.whitebox
import scala.language.experimental.macros
import scala.language.implicitConversions

/** A type class that converts a mix-in type to the greatest lower bound of the components of the mix-in.
  *
  * This [[Glb]] type class provides an extension method [[glb]] via implicit view,
  * which can be import as follow:
  * {{{
  * import com.thoughtworks.feature.Glb.ops._
  * }}}
  *
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  *
  * @example Given a mix-in of two refinement components,
  *
  *          {{{
  *          type A = Option[Any] {
  *            def get: Immutable
  *          }
  *          type B = Option[Any] {
  *            def get: Iterable[String]
  *          }
  *
  *          val ab: A with B = Some(List("cool"))
  *          }}}
  *
  *          when convert it to its greatest lower bound,
  *
  *          {{{
  *          val glbOfAB  = toAllGlbOps(ab).glb
  *          }}}
  *
  *          Then the common method should return the greatest lower bound of each components's return type.
  *
  *          {{{
  *          val result: Immutable with Iterable[String] = glbOfAB.get
  *
  *          result should be(List("cool"))
  *          }}}
  *
  * @note The implicit conversion [[com.thoughtworks.feature.Glb.ops.toAllGlbOps toAllGlbOps]]
  *       must be explicitly invoked, as a workaround of [[https://github.com/scala/bug/issues/10671]]
  *
  * @note This [[Glb]] type class is implemented via [[scala.reflect.api.Types.glb]],
  *       which is similar to [[http://dotty.epfl.ch/docs/reference/intersection-types.html intersection operator]]
  *       in Dotty, except this [[Glb]] works for refinement type only.
  *
  */
@typeclass
trait Glb[Mixin] {
  type Out >: Mixin <: Mixin

  @inline
  final def glb(mixin: Mixin): Out = mixin
}

object Glb {

  type Aux[Mixin, Out0] = Glb[Mixin] {
    type Out = Out0
  }

  /** For internal use only!
    */
  val typeClassCache: Glb.Aux[Any, Any] = new Glb[Any] {
    type Out = Any
  }

  implicit def materialize[Mixin]: Glb[Mixin] = macro Macros.materialize[Mixin]

  private[Glb] final class Macros(val c: whitebox.Context) {
    import c.universe._

    def materialize[Mixin: WeakTypeTag]: Tree = {
      val mixin = weakTypeOf[Mixin]

      val superTypeListBuilder = List.newBuilder[Type]

      def buildDuckList(t: Type): Unit = {
        val dealiased = t.dealias
        dealiased match {
          case RefinedType(superTypes, refinedScope) if refinedScope.isEmpty =>
            superTypes.foreach(buildDuckList)
          case _ =>
            superTypeListBuilder += dealiased
        }
      }

      buildDuckList(mixin)
      val superTypes: List[Type] = superTypeListBuilder.result()

      val out = glb(superTypes)
      q"""_root_.com.thoughtworks.feature.Glb.typeClassCache.asInstanceOf[
        _root_.com.thoughtworks.feature.Glb.Aux[$mixin, $out]
      ]"""

    }

  }

}
