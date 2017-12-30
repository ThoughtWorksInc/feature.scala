package com.thoughtworks.feature
import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import com.thoughtworks.Extractor._

import scala.annotation.meta.getter
import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.collection.generic.Growable
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
  *         @inject val innerFactory: Factory.UnaryByName[AbstractParameter, Inner]
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
  *          If the trait does not contain abstract methods other than `@inject` methods,
  *          then the factory type class is a [[Factory.Nullary]],
  *          which can be summoned by [[Predef.implicitly]],
  *
  *          {{{
  *          val nullaryFactory = implicitly[Factory.Nullary[Foo[Int]]]
  *          }}}
  *
  *          and [[newInstance]] method is available on the [[Factory.Nullary]] as well.
  *
  *          {{{
  *          nullaryFactory.newInstance().orderingA should be(implicitly[Ordering[Int]])
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
  * @note This [[Factory]] disallows creating types that has an abstract member whose type depends on nested types
  *
  *       {{{
  *       trait Outer {
  *         trait Inner
  *         val inner: Option[Inner]
  *       }
  *
  *       "Factory.newInstance[Outer](inner = None)" shouldNot typeCheck
  *       }}}
  *
  * @note However, if the nested type is an alias to another type outside of the type to create, then it is allowed
  *
  *       {{{
  *       trait Outer {
  *         type Inner = String
  *         val inner: Option[Inner]
  *       }
  *
  *       val outer: Outer = Factory[Outer].newInstance(inner = Some("my value"))
  *       outer.inner should be(Some("my value"))
  *       }}}
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

private[feature] trait LowPriorityFactory {
  implicit def factoryLt[Output, Constructor0, LubConstructor](
      implicit factory: Factory.Aux[Output, Constructor0],
      asLt: Factory.Aux[Output, Constructor0] <:< Factory.Lt[Output, LubConstructor]
  ): Factory.Lt[Output, LubConstructor] = {
    asLt(factory)
  }
}

object Factory extends LowPriorityFactory {

  @getter
  final class inject extends StaticAnnotation

  type Aux[Output, Constructor0] = Factory[Output] {
    type Constructor = Constructor0
  }

  type Lt[Output, +Constructor0] = Factory[Output] {
    type Constructor <: Constructor0
  }

  type Nullary[Output] = Lt[Output, () => Output]

  type Unary[-Parameter, Output] = Lt[Output, Parameter => Output]
  type UnaryByName[-Parameter, Output] = Lt[Output, (=> Parameter) => Output]

  def make[Output, Constructor0](constructor: Constructor0): Factory.Aux[Output, Constructor0] = new Factory[Output] {
    type Constructor = Constructor0
    val newInstance: Constructor0 = constructor
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
    private def selfTypes(t: Type): List[Type] = {
      val builder = new ListBuffer[Type]
      val parts = scala.collection.mutable.HashSet.empty[Type]
      def go(t: Type): Unit = {
        val dealiased = t.dealias
        if (!parts(dealiased)) {
          parts += dealiased
          dealiased match {
            case RefinedType(superTypes, refinedScope) if refinedScope.isEmpty =>
              superTypes.foreach(go)
            case typeRef: TypeRef =>
              val symbol = dealiased.typeSymbol
              if (symbol.isClass) {
                val selfType = symbol.asClass.selfType.asSeenFrom(dealiased, symbol)
                go(selfType)
              }
              builder += dealiased
            case _ =>
              builder += dealiased
          }
        }
      }
      go(t)
      builder.result()
    }

    private def demixin(t: Type): List[Type] = {
      val builder = new ListBuffer[Type]
      def go(t: Type): Unit = {
        val dealiased = t.dealias
        dealiased match {
          case RefinedType(superTypes, refinedScope) if refinedScope.isEmpty =>
            for (superType <- superTypes) {
              go(superType)
            }
          case _ =>
            builder += dealiased
        }
      }
      go(t)
      builder.result()
    }
    private val injectType = typeOf[inject]

    private def isAbstractType[Output: WeakTypeTag](symbol: c.universe.TypeSymbol): Boolean = {
      symbol.isAbstract && !symbol.isClass
    }

    def apply[Output: WeakTypeTag]: Tree = {
      val output = weakTypeOf[Output]

      val componentTypes = demixin(glb(selfTypes(output)))

      val linearOutput = internal.refinedType(componentTypes, c.internal.enclosingOwner)
      val linearSymbol = linearOutput.typeSymbol
      val linearThis = internal.thisType(linearSymbol)

      val mixinClassName = c.freshName(c.internal.enclosingOwner.name.encodedName.toTypeName)

      class ThisUntyper extends Untyper[c.universe.type](c.universe) {
        private def replaceThisValue: PartialFunction[Type, Tree] = {
          case tt @ ThisType(symbol) if symbol == linearSymbol =>
            This(mixinClassName)
        }
        override def singletonValue: PartialFunction[Type, Tree] = {
          replaceThisValue.orElse(super.singletonValue)
        }
      }

      def untyper = new ThisUntyper

      def dealiasUntyper = new ThisUntyper {
        override protected def preprocess(tpe: Type): Type = tpe.dealias
      }

      val injectedNames = (for {
        baseClass <- linearOutput.baseClasses.reverse
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
        val memberSymbol = linearOutput.member(methodName).asTerm
        val methodType = memberSymbol.infoIn(linearThis)
        val resultTypeTree: Tree = untyper.untype(methodType.finalResultType)

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
                q"implicit val ${argumentSymbol.name.toTermName}: ${untyper.untype(argumentSymbol.info)}"
              } else {
                q"val ${argumentSymbol.name.toTermName}: ${untyper.untype(argumentSymbol.info)}"
              }
            })
            val typeParameterTrees = methodType.typeParams.map { typeParamSymbol =>
              untyper.typeDefinition(linearThis)(typeParamSymbol.asType)
            }
            q"""
            $modifiers def $methodName[..$typeParameterTrees](...$argumentTrees) = {
              val $methodName = ()
              _root_.com.thoughtworks.feature.The.apply[$resultTypeTree].value
            }
            """
          }
        //          c.info(c.enclosingPosition, show(result), true)
        result
      }

      val zippedProxies: Iterable[(Tree, Tree, Tree, Tree)] = for {
        member <- linearOutput.members
        if !injectedNames(member.name) && member.isTerm && member.isAbstract && !member.asTerm.isSetter
      } yield {
        val memberSymbol = member.asTerm
        val methodName = memberSymbol.name.toTermName
        val argumentName = c.freshName(methodName)
        val methodType = memberSymbol.infoIn(linearThis)
        val resultTypeTree: Tree = dealiasUntyper.untype(methodType.finalResultType)
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
          val (argumentTrees: List[List[ValDef]],
               argumentTypeTrees: List[List[Tree]],
               argumentIdTrees: List[List[Ident]]) =
            methodType.paramLists.map { parameterList =>
              parameterList.map { argumentSymbol =>
                val argumentTypeTree: Tree = dealiasUntyper.untype(argumentSymbol.info)
                val argumentName = argumentSymbol.name.toTermName
                val argumentTree = if (argumentSymbol.asTerm.isImplicit) {
                  q"implicit val $argumentName: $argumentTypeTree"
                } else {
                  q"val $argumentName: $argumentTypeTree"
                }
                (argumentTree, argumentTypeTree, Ident(argumentName))
              }.unzip3
            }.unzip3
          val functionTypeTree = if (argumentTypeTrees.isEmpty) {
            tq"${definitions.ByNameParamClass}[$resultTypeTree]"
          } else {
            argumentTypeTrees.foldRight(resultTypeTree) { (arguments, result) =>
              tq"..$arguments => $result"
            }
          }
          val typeParameterTrees = methodType.typeParams.map { typeParamSymbol =>
            untyper.typeDefinition(linearThis)(typeParamSymbol.asType)
          }
          (q"override def $methodName[..$typeParameterTrees](...$argumentTrees) = $argumentName",
           functionTypeTree,
           q"val $argumentName: $functionTypeTree",
           q"val $methodName: $functionTypeTree")
        }
      }

      val (proxies, parameterTypeTrees, parameterTrees, refinedTree) = zippedProxies.unzip4
      val (defProxies, valProxies) = proxies.partition(_.isDef)
      val typeMembers = for {
        componentType <- componentTypes
        member <- componentType.members
        if member.isType
      } yield member.asType

      val groupedTypeSymbols = typeMembers.groupBy(_.name.encodedName.toTypeName)

      def overrideType(name: TypeName, members: List[TypeSymbol]): Tree = {
        val glbType = glb(members.map { memberSymbol =>
          memberSymbol.infoIn(linearThis)
        })
        val typeParameterTrees = glbType.typeParams.map { typeParamSymbol =>
          untyper.typeDefinition(linearThis)(typeParamSymbol.asType)
        }
        val TypeBounds(_, lowerBound) = glbType.resultType
        val result = q"override type $name[..$typeParameterTrees] = ${untyper.untype(lowerBound)}"
        //          c.info(c.enclosingPosition, show(result), true)
        result
      }

      val overridenTypes = for {
        (name, members) <- groupedTypeSymbols
        if members.forall(isAbstractType)
      } yield overrideType(name, members)

      val makeNew = TermName(c.freshName("makeNew"))
      val constructorMethod = TermName(c.freshName("constructor"))
      val newInstance = TermName(c.freshName("newInstance"))
      val refinedOutput = TypeName(c.freshName("RefinedOutput"))
      val result = q"""
        def $makeNew[$refinedOutput]($newInstance: (..$parameterTypeTrees) => $refinedOutput) =
          _root_.com.thoughtworks.feature.Factory.make[$output, ((..$parameterTypeTrees) => $refinedOutput) {
            def apply(..$refinedTree): $refinedOutput
          }]($newInstance)

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
        $makeNew($constructorMethod _)
      """
//            c.info(c.enclosingPosition, show(result), true)
      result
    }

  }

}
