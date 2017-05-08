package com.thoughtworks

import macrocompat.bundle

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class Constructor[F](val newInstance: F) extends AnyVal

object Constructor {

  def apply[F](implicit constructor: Constructor[F]): Constructor[F] = constructor

  implicit def materialize[F]: Constructor[F] = macro Macros.materialize[F]

  @bundle
  final class Macros(val c: blackbox.Context) {
    import c.universe._

    private def demixin(t: Type): Stream[Type] = {
      t.dealias match {
        case RefinedType(superTypes, refinedScope) if refinedScope.isEmpty =>
          superTypes.toStream.flatMap(demixin)
        case notRefinedType =>
          notRefinedType #:: Stream.Empty
      }
    }

    def materialize[F: WeakTypeTag]: Tree = {
      weakTypeOf[F].dealias match {
        case TypeRef(_, functionSymbol, argumentTypes :+ returnType)
            if functionSymbol == definitions.FunctionClass(argumentTypes.length) =>
          val (argumentIdentiers, argumentDefinitions) = (for (argumentType <- argumentTypes) yield {
            val name = TermName(c.freshName("argument"))
            q"$name" -> q"val $name: $argumentType"
          }).unzip

          demixin(returnType) match {
            case Stream(classType) if !classType.typeSymbol.isAbstract =>
              q"""
                new _root_.com.thoughtworks.Constructor(..$argumentDefinitions =>
                  new $classType(..$argumentIdentiers)
                )
              """
            case classType +: traitTypes =>
              val traitTrees = for (traitType <- traitTypes) yield {
                tq"$traitType"
              }
              q"""
                new _root_.com.thoughtworks.Constructor(..$argumentDefinitions =>
                  new ..${q"$classType(..$argumentIdentiers)" +: traitTrees} {}
                )
              """
          }
        case _ =>
          c.error(c.enclosingPosition, "Expect a function type")
          q"???"
      }
    }
  }

}
