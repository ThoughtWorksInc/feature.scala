package com.thoughtworks.feature
import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import com.thoughtworks.Extractor._

import scala.annotation.meta.getter
import scala.annotation.{StaticAnnotation, compileTimeOnly}

/** A factory to create new instances, especially dynamic mix-ins.
  *
  * @note Factories may be nested
  *
  *       {{{
  *       import com.thoughtworks.feature.Factory.inject
  *       trait Outer {
  *         trait AbstractParameterApi
  *         type AbstractParameter <: AbstractParameterApi
  *
  *         trait InnerApi {
  *           def foo: AbstractParameter
  *         }
  *         type Inner <: InnerApi
  *
  *         @inject val innerFactory: Factory[Inner]
  *       }
  *
  *       Factory[Outer]
  *       }}}
  *
  * @note [[Factory.inject @inject]] works on implicit abstract methods as well.
  *
  *       {{{
  *       import com.thoughtworks.feature.Factory.inject
  *       trait Foo[A] {
  *         @inject implicit def orderingA: Ordering[A]
  *       }
  *       Factory[Foo[Int]].newInstance().orderingA should be(implicitly[Ordering[Int]])
  *       }}}
  *
  * @example Given a trait that contains an abstract method annotated as [[Factory.inject @inject]].
  *
  *          {{{
  *          import com.thoughtworks.feature.Factory.inject
  *          trait Foo[A] {
  *            @inject def orderingA: Ordering[A]
  *          }
  *          }}}
  *
  *          When creating a factory for the trait
  *
  *          {{{
  *          val factory = Factory[Foo[Int]]
  *          }}}
  *
  *          Then the `@inject` method will be replaced to an implicit value.
  *
  *          {{{
  *          val foo = factory.newInstance()
  *          foo.orderingA should be(implicitly[Ordering[Int]])
  *          }}}
  *
  *          It will not compile if no implicit value found.
  *
  *          For example, `Foo[Symbol]` requires an implicit value of type `Ordering[Symbol]`, which is not availble.
  *
  *          {{{
  *          "Factory[Foo[Symbol]]" shouldNot compile
  *          }}}
  *
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
  *          Then the [[newInstance]] method of the factory should accept no parameters.
  *
  *          {{{
  *          val fooBar: Foo with Bar = factory.newInstance()
  *          fooBar should be(a[Foo])
  *          fooBar should be(a[Bar])
  *          }}}
  *
  * @example Given a trait that has abstract members.
  *          {{{
  *          trait Foo {
  *            val bar: Int
  *            var baz: Long
  *          }
  *          }}}
  *          When creating a factory for the trait.
  *          {{{
  *          val factory = Factory[Foo]
  *          }}}
  *          Then the [[newInstance]] method of the factory should accept parameters according to abstract members.
  *          {{{
  *          val foo: Foo = factory.newInstance(bar = 1, baz = 2L)
  *          foo.bar should be(1)
  *          foo.baz should be(2L)
  *          }}}
  *
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
trait Factory[Output] extends Serializable {

  /** A function type that returns `Output`.
    *
    * The parameter types are all abstract members in `Output`
    */
  type Constructor

  /** Returns an instance of `Output`, which overrides abstract members in `Output` according to parameters pass to this [[newInstance]] method. */
  val newInstance: Constructor
}

object Factory {

  @getter
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
      val injectedNames = (for {
        baseClass <- output.baseClasses.reverse
        member <- baseClass.info.decls
        if member.isTerm && {
          internal.initialize(member)
          member.annotations.exists { a =>
            a.tree.tpe <:< injectType
          }
        }
      } yield member.name)(collection.breakOut(Set.canBuildFrom))

      val injects = for {
        injectedName <- injectedNames
      } yield {
        val methodName = injectedName.toTermName
        val memberSymbol = output.member(methodName).asTerm
        val methodType = memberSymbol.info
        val untyper = new OverrideUntyper(memberSymbol.owner)
        val resultTypeTree = untyper.untype(methodType.finalResultType)

        val modifiers = Modifiers(
          Flag.OVERRIDE |
            (if (memberSymbol.isImplicit) Flag.IMPLICIT else NoFlags) |
            (if (memberSymbol.isLazy) Flag.LAZY else NoFlags)
        )
        val result =
          if (memberSymbol.isVar || memberSymbol.setter != NoSymbol) {
            q"""$modifiers var $methodName = {
              val $methodName = ()
              _root_.com.thoughtworks.feature.The.apply[$resultTypeTree].value
            }
            """
          } else if (memberSymbol.isVal || memberSymbol.isGetter || memberSymbol.isStable) {
            q"""
            $modifiers val $methodName = {
               val $methodName = ()
               _root_.com.thoughtworks.feature.The.apply[$resultTypeTree].value
            }
            """
          } else {
            val argumentTrees = methodType.paramLists.map(_.map { argumentSymbol =>
              if (argumentSymbol.asTerm.isImplicit) {
                q"implicit val ${argumentSymbol.name.toTermName}: ${untyper
                  .untype(argumentSymbol.info)}"
              } else {
                q"val ${argumentSymbol.name.toTermName}: ${untyper.untype(argumentSymbol.info)}"
              }
            })
            q"""
            $modifiers def $methodName[..${methodType.typeArgs}](...$argumentTrees) = {
              val $methodName = ()
              _root_.com.thoughtworks.feature.The.apply[$resultTypeTree].value
            }
            """
          }
        //          c.info(c.enclosingPosition, show(result), true)
        result
      }
      val (proxies, parameterTypeTrees, parameterTrees, refinedTree) = (for {
        member <- output.members
        if !injectedNames(member.name) && member.isTerm && member.isAbstract && !member.asTerm.isSetter
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
          val (argumentTrees, argumentTypeTrees, argumentIdTrees) =
            methodType.paramLists
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
      val (defProxies, valProxies) = proxies.partition(_.isDef)
      val overridenTypes =
        (for {
          componentType <- componentTypes
          member <- componentType.members
          if member.isType
        } yield member).distinct
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
              val lowerBounds: List[Tree] = members.collect(scala.Function.unlift[Symbol, Tree] { memberSymbol =>
                val TypeBounds(_, lowerBound) = memberSymbol.info
                if (lowerBound =:= definitions.AnyTpe) {
                  None
                } else {
                  val untyper = new OverrideUntyper(memberSymbol.owner)
                  Some(untyper.untype(lowerBound))
                }
              })(collection.breakOut(List.canBuildFrom))
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
        final class $mixinClassName extends {
          ..$overridenTypes
          ..$valProxies
        } with ..$componentTypes {
          ..$defProxies
          ..$injects
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
