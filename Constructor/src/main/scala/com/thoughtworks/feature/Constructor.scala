package com.thoughtworks.feature

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/** An implicit value for dynamically creating classes and traits, especially dynamic mixins.
  *
  * == Usage ==
  * {{{
  * libraryDependencies += "com.thoughtworks.feature" %% "constructor" % "latest.release"
  * }}}
  * {{{
  * trait A
  * trait B
  *
  * def makeAWithB()(implicit constructor: Constructor[() => A with B]): A with B = {
  *   constructor.newInstance()
  * }
  *
  * val ab: A with B = makeAWithB()
  * }}}

  * == Motivation ==
  *
  * This feature is useful for library authors.
  * A library author may ask his user to create a `trait` type, then dynamically mix-in it with the features provided by the library.
  *
  * Suppose you are creating a [[https://martinfowler.com/bliki/DomainSpecificLanguage.html DSL]] that compiles to JavaScript.
  *
  * You want your DSL is extensible.
  * For example, the DSL users should be able to create custom binary operators.
  *
  * With the help of `Constructor.scala`, you can put the boilerplate code into a private class `BinaryOperator`:
  *
  * {{{
  * trait Ast
  *
  * object Ast {
  *
  *   class Literal(val n: Int) extends Ast {
  *     override final def compile(): String = n.compile()
  *   }
  *
  *   private[Ast] abstract class BinaryOperator(leftHandSide: Ast, rightHandSide: Ast) extends Ast {
  *     protected def symbol: String
  *     override final def compile() = s"(\$leftHandSide \$symbol \$rightHandSide)"
  *   }
  *
  *   def binaryOperator[T](leftHandSide: Ast, rightHandSide: Ast)(
  *     implicit constructor: Constructor[(Ast, Ast) => BinaryOperator with T]): BinaryOperator with T = {
  *     constructor.newInstance(leftHandSide, rightHandSide)
  *   }
  *
  * }
  * }}}
  *
  * The users only need a very simple implementation for their custom binary operators.
  *
  * {{{
  * import Ast._
  *
  * trait Plus {
  *   protected final def symbol = "+"
  * }
  *
  * trait Minus {
  *   protected final def symbol = "-"
  * }
  *
  * val myAst = binaryOperator[Plus](
  *   new Literal(1),
  *   binaryOperator[Minus](
  *     new Literal(3),
  *     new Literal(5)
  *   )
  * )
  *
  * print(myAst.compile()) // Output: "(1 + (3 - 5))"
  * }}}
  *
  * == An alternative approach ==
  *
  * There is another approach to integrate partial implementation from users: asking users to provide custom callback functions or type classes.
  *
  * However, the callback functions or type classes approach will create additional object instances and additional references for each instance at run-time.
  * On the other hand, the `Constructor.scala` approach create classes at compile-time and no additional run-time references.
  * As a result, at run-time, `Constructor.scala` approach will consume less memory, and performs less indirect access on memory.
  *
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class Constructor[F](val newInstance: F) extends AnyVal

object Constructor {

  implicit def apply[F]: Constructor[F] = macro Macros.apply[F]

  final class Macros(val c: blackbox.Context) {
    import c.universe._

    private def demixin(t: Type): Stream[Type] = {
      t.dealias match {
        case RefinedType(superTypes, refinedScope) if refinedScope.isEmpty =>
          superTypes.toStream.flatMap(demixin)
        case notRefinedType =>
          Stream(notRefinedType)
      }
    }

    def apply[F: WeakTypeTag]: Tree = {
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
                new _root_.com.thoughtworks.feature.Constructor(..$argumentDefinitions =>
                  new $classType(..$argumentIdentiers)
                )
              """
            case classType +: traitTypes =>
              val traitTrees = for (traitType <- traitTypes) yield {
                tq"$traitType"
              }
              q"""
                new _root_.com.thoughtworks.feature.Constructor(..$argumentDefinitions =>
                  new ..${(q"$classType(..$argumentIdentiers)" +: traitTrees).toSeq /* toSeq is a workaround for Scala 2.10 */} {}
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
