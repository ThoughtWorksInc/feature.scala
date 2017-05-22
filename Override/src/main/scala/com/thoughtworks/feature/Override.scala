package com.thoughtworks.feature
import com.thoughtworks.Extractor._
import shapeless.PolyDefns.~>
import shapeless._

import scala.annotation.StaticAnnotation
import scala.collection.immutable.Queue
import scala.language.dynamics
import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.util.control.NonFatal
import scala.language.existentials

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
abstract class Override[Vals, Super] extends Dynamic {
  def applyDynamic(method: String)(): Any = macro RecordMacros.forwardImpl

  /**
    * @usecase def newInstance(vals: Any*): Super = ???
    */
  def applyDynamicNamed(method: String)(rec: Any*): Any =
    macro RecordMacros.forwardNamedImpl

  type Out <: Super
  def newInstanceRecord(vals: Vals): Out
}

object Override {
  type Aux[Vals, Super, Out0] = Override[Vals, Super] {
    type Out = Out0
  }

  final class inject extends StaticAnnotation

  private[Override] final class PartiallyAppliedNewInstance[Super] extends Dynamic {
    def applyRecord[Vals, Out <: Super](vals: Vals)(implicit `override`: Override.Aux[Vals, Super, Out]): Out = {
      `override`.newInstanceRecord(vals)
    }

    def applyDynamic[Issues10307Workaround](method: String)(): Any =
      macro RecordMacros.forwardImpl

    def applyDynamicNamed[Issues10307Workaround](method: String)(rec: Any*): Any = macro RecordMacros.forwardNamedImpl
  }

  /** @usecase def newInstance[Super](vals: Any*): Super = ???
    */
  def newInstance[Super]: PartiallyAppliedNewInstance[Super] =
    new PartiallyAppliedNewInstance[Super]

  def apply[Vals, Super](implicit `override`: Override[Vals, Super]): Override.Aux[Vals, Super, `override`.Out] =
    `override`

  implicit def materialize[Vals, Super]: Override[Vals, Super] =
    macro Macros.materialize[Vals, Super]

  private[Override] final class Macros(val c: whitebox.Context) extends CaseClassMacros with SingletonTypeUtils {
    import c.universe._

    private val injectType = typeOf[inject]
    private def demixin(t: Type): Stream[Type] = {
      t.dealias match {
        case RefinedType(superTypes, refinedScope) if refinedScope.isEmpty =>
          superTypes.toStream.flatMap(demixin)
        case notRefinedType =>
          Stream(notRefinedType)
      }
    }

    def materialize[Vals: WeakTypeTag, Super: WeakTypeTag]: Tree =
      try {
        val mixinType = weakTypeOf[Super]
        val valsType = weakTypeOf[Vals]
        val valTypes = unpackHListTpe(valsType)
        object DealiasFieldType {
          def unapply(arg: Type): Option[(String, Type)] = arg.dealias match {
            case FieldType(keyType, v) =>
              keyType.dealias match {
                case SingletonSymbolType(k) =>
                  Some(k, v)
                case _ => None
              }
            case _ => None
          }
        }
        val (temporaryNames, upvalues) =
          (for (DealiasFieldType(k, v) <- valTypes) yield {
            val overridingName = TermName(k)
            val setters = for {
              alternative <- mixinType.member(overridingName).alternatives
              setter = alternative.asMethod.setter
              if setter != NoSymbol
            } yield setter
            val temporaryName = TermName(c.freshName())
            val upvalue =
              if (setters.isEmpty || setters.exists(!_.isAbstract)) {
                q"override val $overridingName: $v = $temporaryName"
              } else {
                q"override var $overridingName: $v = $temporaryName"
              }
            (temporaryName, upvalue)
          }).unzip
        val pattern = valTypes.view
          .zip(temporaryNames)
          .foldRight[Tree](pq"_root_.shapeless.HNil") {
            case ((DealiasFieldType(k, v), temporaryName), accumulator) =>
              pq"_root_.shapeless.::($temporaryName, $accumulator)"
          }
        val superTypes: Stream[c.universe.Type] = demixin(mixinType)

        val superTrees = for (superType <- superTypes.distinct) yield {
          tq"$superType"
        }
        val mixinClassName = TypeName(c.freshName("Mixin"))

        val argumentHListName = TermName(c.freshName("argumentHList"))

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
                  .asSeenFrom(mixinType, baseClass))}[..${typeArguments}]"
            }
          }

          override def untype: PartialFunction[Type, Tree] = {
            replaceTypeArguments.orElse(super.untype)
          }
        }
        val injects = for {
          baseClass <- mixinType.baseClasses.reverse
          member <- baseClass.info.decls
          if member.isTerm && {
            internal.initialize(member)
            member.annotations.exists { a =>
              a.tree.tpe <:< injectType
            }
          }
        } yield {
          val memberSymbol = member.asTerm
          val methodName = memberSymbol.name.toTermName
          val methodType = memberSymbol.info
          val untyper = new OverrideUntyper(baseClass)
          val resultTypeTree = untyper.untype(methodType.finalResultType)
          val result =
            if (memberSymbol.isVar || memberSymbol.isSetter || memberSymbol.setter != NoSymbol) {
              q"override var $methodName = _root_.shapeless.the.apply[$resultTypeTree]"
            } else if (memberSymbol.isVal || memberSymbol.isGetter || memberSymbol.isStable) {
              q"override val $methodName = _root_.shapeless.the.apply[$resultTypeTree]"
            } else {
              val argumentTrees = methodType.paramLists.map(_.map { argumentSymbol =>
                if (argumentSymbol.asTerm.isImplicit) {
                  q"implicit val ${argumentSymbol.name.toTermName}: ${untyper
                    .untype(argumentSymbol.info)}"
                } else {
                  q"val ${argumentSymbol.name.toTermName}: ${untyper.untype(argumentSymbol.info)}"
                }
              })
              q"override def $methodName[..${methodType.typeArgs}](...$argumentTrees) = _root_.shapeless.the.apply[$resultTypeTree]"
            }
//          c.info(c.enclosingPosition, show(result), true)
          result
        }
        val overridenTypes =
          (for {
            baseClass <- mixinType.baseClasses.reverse
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
        val result = q"""
          @_root_.scala.inline def fromFunction[Out0 <: $mixinType](f: $valsType => Out0): Override.Aux[$valsType, $mixinType, Out0] = new Override[$valsType, $mixinType] {
            type Out = Out0
            @_root_.scala.inline override final def newInstanceRecord(vals: $valsType) = f(vals)
          }
          val f = { (_: $valsType) match {
            case $pattern =>
              final class $mixinClassName extends ..$superTrees {
                ..$upvalues
                ..$overridenTypes
                ..$injects
              }
              new $mixinClassName
            }
          }
          fromFunction(f)
        """
//        c.info(c.enclosingPosition, showCode(result), true)
        result
      } catch {
        case NonFatal(e) =>
          e.printStackTrace()
          throw e
      }
  }
}
