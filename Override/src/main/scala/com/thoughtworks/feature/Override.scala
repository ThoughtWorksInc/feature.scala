package com.thoughtworks.feature
import com.thoughtworks.Extractor._
import com.thoughtworks.feature.DelayMacros.DelayTreeCreator
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
final class Override[Vals, Result](val newInstanceRecord: Vals => Result) extends AnyVal with Dynamic {
  def applyDynamic(method: String)(): Any = macro RecordMacros.forwardImpl

  /**
    * @usecase def newInstance(vals: Any*): Result = ???
    */
  def applyDynamicNamed(method: String)(rec: Any*): Any = macro RecordMacros.forwardNamedImpl
}

object Override {

  final class inject extends StaticAnnotation

  private[Override] final class PartiallyAppliedNewInstance[Result] extends Dynamic {
    def applyRecord[Vals](vals: Vals)(implicit cachedOverride: Override[Vals, Result]): Result = {
      cachedOverride.newInstanceRecord(vals)
    }

    def applyDynamic[Issues10307Workaround](method: String)(): Any = macro RecordMacros.forwardImpl

    def applyDynamicNamed[Issues10307Workaround](method: String)(rec: Any*): Any = macro RecordMacros.forwardNamedImpl
  }

  /** @usecase def newInstance[Result](vals: Any*): Result = ???
    */
  def newInstance[Result]: PartiallyAppliedNewInstance[Result] = new PartiallyAppliedNewInstance[Result]

  def apply[Vals, Result](implicit `override`: Override[Vals, Result]): Override[Vals, Result] = `override`

  implicit def materialize[Vals, Result]: Override[Vals, Result] = macro Macros.materialize[Vals, Result]

  private[Override] final class Macros(val c: whitebox.Context)
      extends CaseClassMacros
      with SingletonTypeUtils
      with DelayMacros {
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

    def materialize[Vals: WeakTypeTag, Result: WeakTypeTag]: Tree =
      try {
        val mixinType = weakTypeOf[Result]
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
        val (temporaryNames, upvalues) = (for (DealiasFieldType(k, v) <- valTypes) yield {
          val overridingName = TermName(k)
          val setters = for {
            alternative <- mixinType.member(overridingName).alternatives
            setter = alternative.asMethod.setter
            if setter != NoSymbol
          } yield setter
          val temporaryName = TermName(c.freshName())
          val upvalue = if (setters.isEmpty || setters.exists(!_.isAbstract)) {
            q"override val $overridingName: $v = $temporaryName"
          } else {
            q"override var $overridingName: $v = $temporaryName"
          }
          (temporaryName, upvalue)
        }).unzip
        val pattern = valTypes.view.zip(temporaryNames).foldRight[Tree](pq"_root_.shapeless.HNil") {
          case ((DealiasFieldType(k, v), temporaryName), accumulator) =>
            pq"_root_.shapeless.::($temporaryName, $accumulator)"
        }
        val superTypes: Stream[c.universe.Type] = demixin(mixinType)

        val superTrees = for (superType <- superTypes) yield {
          tq"$superType"
        }
        val mixinClassName = TypeName(c.freshName("Mixin"))

        val argumentHListName = TermName(c.freshName("argumentHList"))

        def untyper(baseClass: Symbol) = new Untyper[c.universe.type](c.universe) {
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
                TypeApply(
                  super.untype(
                    internal
                      .typeRef(NoPrefix, s, Nil)
                      .asSeenFrom(mixinType.asInstanceOf[Type], baseClass.asInstanceOf[Symbol])),
                  typeArguments.toList
                )
            }
          }

          override def untype: PartialFunction[Type, Tree] = {
            replaceTypeArguments.orElse(super.untype)
          }
        }
        val injects = for {
          baseClass <- mixinType.baseClasses.reverse
          member <- baseClass.info.decls
          if member.isMethod && {
            internal.initialize(member.asInstanceOf[Symbol])
            member.annotations.exists { a =>
              a.tree.tpe <:< injectType
            }
          }
        } yield {
          val memberSymbol = member.asInstanceOf[Symbol].asMethod
          val methodName = memberSymbol.name.toTermName
          val methodType = memberSymbol.info
          val resultTypeTree = untyper(baseClass.asInstanceOf[Symbol]).untype(methodType.finalResultType)
          val result = if (memberSymbol.isVar || memberSymbol.isSetter || memberSymbol.setter != NoSymbol) {
            q"override var $methodName = _root_.shapeless.the.apply[$resultTypeTree]"
          } else if (memberSymbol.isVal || memberSymbol.isGetter || memberSymbol.isStable) {
            q"override val $methodName = _root_.shapeless.the.apply[$resultTypeTree]"
          } else {
            val argumentTrees = methodType.paramLists.map(_.map { argumentSymbol =>
              if (argumentSymbol.asTerm.isImplicit) {
                q"implicit val ${argumentSymbol.name.toTermName}: ${argumentSymbol.info}"
              } else {
                q"val ${argumentSymbol.name.toTermName}: ${argumentSymbol.info}"
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
                _.info.asInstanceOf[Type] match {
                  case TypeBounds(_, _) => true
                  case _ => false
                }
              }
            }
            .map {
              case (name, members) =>
                val lowerBounds = members.collect(scala.Function.unlift { member =>
                  val memberSymbol = member
                  val TypeBounds(_, lowerBound) = memberSymbol.info
                  if (lowerBound =:= definitions.AnyTpe) {
                    None
                  } else {
                    Some(untyper(memberSymbol.owner).untype(lowerBound))
                  }
                })
                val compoundTypeTree = CompoundTypeTree(Template(lowerBounds.toList, noSelfType, Nil))
                val result = q"override type ${TypeName(name)} = $compoundTypeTree"
//                c.info(c.enclosingPosition, show(result), true)
                result
            }
        val result = q"""
          new _root_.com.thoughtworks.feature.Override[$valsType, $mixinType]({ case $pattern =>
            final class $mixinClassName extends ..$superTrees {
              ..$upvalues
              ..$overridenTypes
              ..$injects
            }
            new $mixinClassName
          })
        """
//        c.info(c.enclosingPosition, showCode(result), false)
        result
      } catch {
        case NonFatal(e) =>
          e.printStackTrace()
          throw e
      }
  }
}
