package com.thoughtworks.feature
import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import com.thoughtworks.Extractor._

import scala.annotation.StaticAnnotation

/** A factory to create new instances, especially dynamic mix-ins.
  *
  * @note The factory may contain abstract parameters of abstract types
  *
  *       {{{
  *       trait Outer {
  *         trait AbstractParameterApi
  *         type AbstractParameter <: AbstractParameterApi
  *
  *         trait InnerApi {
  *           def foo: AbstractParameter
  *         }
  *         type Inner <: InnerApi
  *       }
  *
  *       new Outer {
  *         type Inner = InnerApi
  *         val innerFactory = Factory[Inner]
  *       }
  *       }}}
  *
  * @example Given two traits that have no abstract member.
  *
  *          {{{
  *          trait Foo
  *          trait Bar
  *          }}}
  *
  *          When creating a factory for mix-in type of the two types.
  *
  *          {{{
  *          val factory = Factory[Foo with Bar]
  *          }}}
  *
  *          Then the newInstance of the factory should accept no parameters.
  *
  *          {{{
  *          val fooBar: Foo with Bar = factory.newInstance()
  *          fooBar should be(a[Foo])
  *          fooBar should be(a[Bar])
  *          }}}
  *
  * @example Given a trait that has an abstract member.
  *          {{{
  *          trait Foo {
  *            var bar: Int
  *          }
  *          }}}
  *          When creating a factory for the trait.
  *          {{{
  *          val factory = Factory[Foo]
  *          }}}
  *          Then the newInstance of the factory should accept one parameter.
  *          {{{
  *          val foo: Foo = factory.newInstance(bar = 1)
  *          foo.bar should be(1)
  *          }}}
  *
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
trait Factory[Output] {
  type Constructor
  val newInstance: Constructor
}

object Factory {

  final class inject extends StaticAnnotation

  type Aux[Output, Constructor0] = Factory[Output] {
    type Constructor = Constructor0
  }

  implicit def apply[Output]: Factory[Output] = macro Macros.apply[Output]

  private[Factory] final class Macros(val c: whitebox.Context) {
    import c.universe._

    implicit final class Unzip4[A, B, C, D](val xs: Iterable[(A, B, C, D)]) {
      def unzip4: (List[A], List[B], List[C], List[D]) =
        xs.foldRight[(List[A], List[B], List[C], List[D])]((Nil, Nil, Nil, Nil)) { (x, res) =>
          val (a, b, c, d) = x
          (a :: res._1, b :: res._2, c :: res._3, d :: res._4)
        }
    }
    private def demixin(t: Type): Stream[Type] = {
      t.dealias match {
        case RefinedType(superTypes, refinedScope) if refinedScope.isEmpty =>
          superTypes.toStream.flatMap(demixin)
        case notRefinedType =>
          Stream(notRefinedType)
      }
    }
    private val injectType = typeOf[inject]

    def apply[Output: WeakTypeTag]: Tree = {
      val output = weakTypeOf[Output]

      val componentTypes = demixin(output).distinct

      val mixinClassName = TypeName(c.freshName("Anonymous"))

      final class OverrideUntyper(baseClass: Symbol) extends Untyper[c.universe.type](c.universe) {
        private def replaceThisValue: PartialFunction[Type, Tree] = {
          case tt @ ThisType(symbol) if symbol == baseClass =>
            This(mixinClassName)
        }
        override def singletonValue: PartialFunction[Type, Tree] = {
          replaceThisValue.orElse(super.singletonValue)
        }
        private def replaceTypeArguments: PartialFunction[Type, Tree] = {
          def superUntype = super.untype;
          {
            case tpe @ TypeRef(NoPrefix, s, superUntype.extract.forall(typeArguments)) =>
              tq"${super.untype(internal
                .typeRef(NoPrefix, s, Nil)
                .asSeenFrom(output, baseClass))}[..$typeArguments]"
          }
        }

        override def untype: PartialFunction[Type, Tree] = {
          replaceTypeArguments.orElse(super.untype)
        }
      }

      val (overridenSymbols, injects) = (for {
        baseClass <- output.baseClasses.reverse
        member <- baseClass.info.decls
        if member.isTerm && {
          internal.initialize(member)
          member.annotations.exists { a =>
            a.tree.tpe <:< injectType
          }
        } && !member.asTerm.isSetter
      } yield {
        val memberSymbol = member.asTerm
        val methodName = memberSymbol.name.toTermName
        val methodType = memberSymbol.info
        val untyper = new OverrideUntyper(baseClass)
        val resultTypeTree = untyper.untype(methodType.finalResultType)

        val modifiers = Modifiers(
          Flag.OVERRIDE |
            (if (memberSymbol.isImplicit) Flag.IMPLICIT else NoFlags) |
            (if (memberSymbol.isLazy) Flag.LAZY else NoFlags)
        )
        val result =
          if (memberSymbol.isVar || memberSymbol.setter != NoSymbol) {
            q"$modifiers var $methodName = _root_.com.thoughtworks.feature.The.apply[$resultTypeTree].value"
          } else if (memberSymbol.isVal || memberSymbol.isGetter || memberSymbol.isStable) {
            q"$modifiers val $methodName = _root_.com.thoughtworks.feature.The.apply[$resultTypeTree].value"
          } else {
            val argumentTrees = methodType.paramLists.map(_.map { argumentSymbol =>
              if (argumentSymbol.asTerm.isImplicit) {
                q"implicit val ${argumentSymbol.name.toTermName}: ${untyper
                  .untype(argumentSymbol.info)}"
              } else {
                q"val ${argumentSymbol.name.toTermName}: ${untyper.untype(argumentSymbol.info)}"
              }
            })
            q"$modifiers def $methodName[..${methodType.typeArgs}](...$argumentTrees) = _root_.com.thoughtworks.feature.The.apply[$resultTypeTree].value"
          }
        //          c.info(c.enclosingPosition, show(result), true)
        memberSymbol -> result
      }).unzip
      val injectedMembers: Map[TermName, Iterable[Symbol]] = overridenSymbols.groupBy(_.name).withDefaultValue(Nil)
      val (proxies, parameterTypeTrees, parameterTrees, refinedTree) = (for {
        member <- output.members
        if member.isTerm && member.isAbstract && injectedMembers(member.name.toTermName).forall { injectedMember =>
          !(injectedMember.infoIn(output) <:< member.infoIn(output))
        } && !member.asTerm.isSetter
      } yield {
        val memberSymbol = member.asTerm
        val methodName = memberSymbol.name.toTermName
        val argumentName = TermName(c.freshName(methodName.toString))
        val methodType = memberSymbol.infoIn(output)
        val untyper = new OverrideUntyper(member.owner)
        val resultTypeTree = untyper.untype(methodType.finalResultType)
        if (memberSymbol.isVar || memberSymbol.setter != NoSymbol) {
          (q"override var $methodName = $argumentName",
            resultTypeTree,
            q"val $argumentName: $resultTypeTree",
            q"val $methodName: $resultTypeTree")
        } else if (memberSymbol.isVal || memberSymbol.isGetter || memberSymbol.isStable) {
          (q"override val $methodName = $argumentName",
            resultTypeTree,
            q"val $argumentName: $resultTypeTree",
            q"val $methodName: $resultTypeTree")
        } else {
          val (argumentTrees, argumentTypeTrees, argumentIdTrees) = methodType.paramLists
            .map(_.map { argumentSymbol =>
              val argumentTypeTree = untyper.untype(argumentSymbol.info)
              val argumentName = argumentSymbol.name.toTermName
              val argumentTree = if (argumentSymbol.asTerm.isImplicit) {
                q"implicit val $argumentName: $argumentTypeTree"
              } else {
                q"val $argumentName: $argumentTypeTree"
              }
              (argumentTree, argumentTypeTree, Ident(argumentName))
            }.unzip3)
            .unzip3
          val functionTypeTree = if (argumentTypeTrees.isEmpty) {
            tq"${definitions.ByNameParamClass}[$resultTypeTree]"
          } else {
            argumentTypeTrees.foldRight(resultTypeTree) { (arguments, result) =>
              tq"..$arguments => $result"
            }
          }
          (q"override def $methodName[..${methodType.typeArgs}](...$argumentTrees) = $argumentName",
            functionTypeTree,
            q"val $argumentName: $functionTypeTree",
            q"val $methodName: $functionTypeTree")
        }
      }).unzip4
      val overridenTypes =
        (for {
          baseClass <- output.baseClasses.reverse
          member <- baseClass.info.decls
          if member.isType
        } yield member)
          .groupBy(_.name.toString)
          .withFilter {
            _._2.forall {
              _.info match {
                case TypeBounds(_, _) => true
                case _ => false
              }
            }
          }
          .map {
            case (name, members) =>
              val lowerBounds = members.collect(scala.Function.unlift[Symbol, Tree] { memberSymbol =>
                val TypeBounds(_, lowerBound) = memberSymbol.info
                if (lowerBound =:= definitions.AnyTpe) {
                  None
                } else {
                  val untyper = new OverrideUntyper(memberSymbol.owner)
                  Some(untyper.untype(lowerBound))
                }
              })
              val typeTree = if (lowerBounds.isEmpty) {
                TypeTree(definitions.AnyTpe)
              } else {
                CompoundTypeTree(Template(lowerBounds, noSelfType, Nil))
              }
              val result = q"override type ${TypeName(name)} = $typeTree"
              //                c.info(c.enclosingPosition, show(result), true)
              result
          }

      val makeNew = TermName(c.freshName("makeNew"))
      val constructorMethod = TermName(c.freshName("constructor"))
      val newInstance = TermName(c.freshName("newInstance"))
      val refinedOutput = TypeName(c.freshName("RefinedOutput"))
      val result = q"""
      def $makeNew[$refinedOutput]($newInstance: (..$parameterTypeTrees) => $refinedOutput): _root_.com.thoughtworks.feature.Factory.Aux[
        $output,
        ((..$parameterTypeTrees) => $refinedOutput) {
          def apply(..$refinedTree): $refinedOutput
        }] = new _root_.com.thoughtworks.feature.Factory[$output] {
        type Constructor = ((..$parameterTypeTrees) => $refinedOutput) {
          def apply(..$refinedTree): $refinedOutput
        }
        override val newInstance: Constructor = $newInstance
      }

      def $constructorMethod(..$parameterTrees) = {
        final class $mixinClassName extends ..$componentTypes {
          ..$overridenTypes
          ..$injects
          ..$proxies
        }
        new $mixinClassName
      }
      val $newInstance = $constructorMethod _
      $makeNew($newInstance)
      """
      //      c.info(c.enclosingPosition, show(result), true)
      result
    }

  }

}
