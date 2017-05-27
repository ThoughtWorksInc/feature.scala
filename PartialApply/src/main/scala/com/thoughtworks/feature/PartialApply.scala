package com.thoughtworks.feature

import scala.reflect.macros.whitebox
import scala.language.dynamics
import scala.language.experimental.macros
import scala.util.control.NonFatal

/** A dependent type class that bind the specific parameter of `ParameterName` to `F`
  *
  * = Imports =
  *
  * {{{
  * import com.thoughtworks.feature.PartialApply._
  * }}}
  *
  * This will enable the [[com.thoughtworks.feature.PartialApply.PartialApplyOps.partialApply partialApply]] method for any functions
  *
  * @tparam F The function type to be partially apply
  *
  * @example Case class companion can partially apply.
  *
  *          {{{
  *          case class MyCaseClass(i: Int, d: Double, s: String)
  *
  *          MyCaseClass.partialApply(s = "seconds").apply(i = 60, d = 1.125) should be(MyCaseClass(s = "seconds", i = 60, d = 1.125))
  *          }}}
  *
  * @example Function objects can partially apply.
  *
  *          {{{
  *          object f extends ((Int, Double, String) => String) {
  *            def apply(i: Int, d: Double, s: String): String = {
  *              (i * d) + s
  *            }
  *          }
  *
  *          f.partialApply(s = "seconds").apply(i = 60, d = 1.125) should be(f(s = "seconds", i = 60, d = 1.125))
  *          }}}
  *
  * @example Partial applying can be chained.
  *
  *          {{{
  *          val f = { (v1: Int, v2: Int, v3: Int) => (v1 + v2) * v3 }
  *
  *          f.partialApply(v2 = 2).partialApply(v3 = 3).partialApply(v1 = 1).apply() should be(f(1, 2, 3))
  *          }}}
  *
  * @example A function with refined parameters can partially apply.
  *
  *          {{{
  *          val f: ((Int, Double, String) => String) { def apply(i: Int, d: Double, s: String): String } = { (i, d, s) =>
  *            (i * d) + s
  *          }
  *
  *          f.partialApply(s = "seconds").apply(i = 60, d = 1.125) should be(f(s = "seconds", i = 60, d = 1.125))
  *          }}}
  *
  * @example Given a function with three parameters.
  *
  *          {{{
  *          val f = { (v1: Int, v2: Int, v3: Int) => (v1 + v2) * v3 }
  *          }}}
  *
  *          When partially applying the second parameter.
  *
  *          {{{
  *          val partiallyApplied = f.partialApply(v2 = 2)
  *          }}}
  *
  *          And applying the rest parameters.
  *
  *          {{{
  *          val result = partiallyApplied(v3 = 3, v1 = 1)
  *          }}}
  *
  *          Then the result should be the same as applying at once.
  *
  *          {{{
  *          result should be(f(1, 2, 3))
  *          }}}
  *
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
trait PartialApply[F, ParameterName <: String with Singleton] {
  type Parameter
  type Rest

  def apply(f: F, parameter: Parameter): Rest
}

object PartialApply {

  type Aux[F, ParameterName <: String with Singleton, Parameter0, Rest0] = PartialApply[F, ParameterName] {
    type Parameter >: Parameter0
    type Rest <: Rest0
  }

  object Runtime {
    private[Runtime] type Id[+A] = A
    def narrow(a: Any): Id[a.type] = a
  }

  implicit final class PartialApplyOps[F](f: F) extends Dynamic {
    def partialApply: this.type = this

    // Workaround for https://github.com/typelevel/scala/issues/151
    def applyDynamicNamed(methodName: String)(pairs: Any*): Any = macro Macros.applyDynamicNamed

    def applyDynamicNarrowNamed[M <: String with Singleton,
                                ParameterName <: String with Singleton,
                                Parameter,
                                RequiredParameter,
                                Rest](methodName: M)(pair: (ParameterName, Parameter))(
        implicit partialApply: PartialApply.Aux[F, ParameterName, RequiredParameter, Rest],
        constraint: Parameter <:< RequiredParameter
    ): Rest = {
      partialApply(f, pair._2)
    }
  }

  def apply[F, ParameterName <: String with Singleton](implicit partialApply: PartialApply[F, ParameterName])
    : PartialApply.Aux[F, ParameterName, partialApply.Parameter, partialApply.Rest] = partialApply

  implicit def materialize[F, ParameterName <: String with Singleton]: PartialApply[F, ParameterName] =
    macro Macros.materialize[F, ParameterName]

  private[feature] final class Macros(val c: whitebox.Context) {
    import c.universe._

    def applyDynamicNamed(methodName: Tree)(pairs: Tree*): Tree = {
      val narrowPairs = pairs.map {
        case q"$tupleConstructor($parameterName, $parameter)" =>
          q"(_root_.com.thoughtworks.feature.PartialApply.Runtime.narrow($parameterName), $parameter)"
      }
      q"${c.prefix.tree}.applyDynamicNarrowNamed($methodName)(..$narrowPairs)"
    }
    def materialize[F: WeakTypeTag, ParameterName <: String with Singleton: WeakTypeTag]: Tree =
      try {

        val f = weakTypeOf[F]
        val parameterLiteralType = weakTypeOf[ParameterName]

        val ConstantType(Constant(parameterNameString: String)) = parameterLiteralType.dealias
        val applySymbol = f.member(TermName("apply")).asMethod
        val MethodType(params, localResult) = applySymbol.info
        val output = localResult.asSeenFrom(f, applySymbol.owner)

        params.partition(_.name.toString == parameterNameString) match {
          case (Seq(), _) =>
            c.error(c.enclosingPosition, s"not found: parameter $parameterNameString")
            q"_root_.scala.Predef.???"
          case (Seq(parameterSymbol), restParameterSymbols) =>
            val parameter = parameterSymbol.info.asSeenFrom(f, applySymbol.owner)
            val functionName = TermName(c.freshName("f"))
            val parameterIdents = params.map(Ident apply _.name)
            val (restParameterTypes, restParameters) = restParameterSymbols.map { restParameterSymbol =>
              val restParameterTypeTree = tq"${restParameterSymbol.info.asSeenFrom(f, applySymbol.owner)}"
              val restParameter = q"val ${TermName(restParameterSymbol.name.toString)}: $restParameterTypeTree"
              (restParameterTypeTree, restParameter)
            }.unzip
            val rest = tq"((..$restParameterTypes) => $output) { def apply(..$restParameters): $output }"
            val result = q"""
              new _root_.com.thoughtworks.feature.PartialApply[$f, $parameterLiteralType] {
                type Parameter = $parameter
                type Rest = $rest
                def apply($functionName: $f, ${TermName(parameterNameString)}: Parameter): Rest = { (..$restParameters) =>
                  $functionName(..$parameterIdents)
                }
              }
            """
//            c.info(c.enclosingPosition, show(result), true)
            result
        }

      } catch {
        case NonFatal(e) =>
          e.printStackTrace()
          throw e
      }

  }
}
