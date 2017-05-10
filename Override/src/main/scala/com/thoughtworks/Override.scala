package com.thoughtworks
import com.thoughtworks.DelayMacros.DelayTreeCreator
import macrocompat.bundle
import shapeless._

import scala.annotation.StaticAnnotation
import scala.collection.immutable.Queue
import scala.language.dynamics
import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.util.control.NonFatal

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
    def applyDynamicNamed[Issues10307Workaround](method: String)(rec: Any*): Any = macro RecordMacros.forwardNamedImpl
  }

  /** @usecase def newInstance[Result](vals: Any*): Result = ???
    */
  def newInstance[Result]: PartiallyAppliedNewInstance[Result] = new PartiallyAppliedNewInstance[Result]

  def apply[Vals, Result](implicit `override`: Override[Vals, Result]): Override[Vals, Result] = `override`

  implicit def materialize[Vals, Result]: Override[Vals, Result] = macro Macros.materialize[Vals, Result]

  @bundle
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
        val valuesType = mkHListTpe(for (DealiasFieldType(_, v) <- valTypes) yield v)
        val argumentHListName = TermName(c.freshName("argumentHList"))

        val injects = for {
          baseClass <- mixinType.baseClasses.reverse
          member <- baseClass.info.decls
          if member.isMethod && {
            c.internal.initialize(member)
            member.annotations.exists { a =>
              a.tree.tpe <:< injectType
            }
          }
        } yield {
          delayValOrDef(new DelayTreeCreator {
            override def apply(c: whitebox.Context): c.universe.Tree =
              try {
                import c.universe._

                val memberSymbol = member.asInstanceOf[Symbol].asMethod
                val methodName = memberSymbol.name.toTermName

//                def  narrowSuperType = internal.superType(internal.thisType(c.internal.enclosingOwner),
//                                                         internal.thisType(memberSymbol.owner))
//
//                val methodType = memberSymbol.infoIn(narrowSuperType) // TODO: need Type To Tree conversion for better this.type
                val methodType = memberSymbol.info
                val result = if (memberSymbol.isVar) {
                  q"override var $methodName = _root_.scala.Predef.implicitly"
                } else if (memberSymbol.isVal) {
                  q"override val $methodName = _root_.scala.Predef.implicitly"
                } else {
                  val argumentTrees = methodType.paramLists.map(_.map { argumentSymbol =>
                    if (argumentSymbol.asTerm.isImplicit) {
                      q"implicit val ${argumentSymbol.name.toTermName}: ${argumentSymbol.info}"
                    } else {
                      q"val ${argumentSymbol.name.toTermName}: ${argumentSymbol.info}"
                    }
                  })
                  q"override def $methodName[..${methodType.typeArgs}](...$argumentTrees) = _root_.scala.Predef.implicitly"
                }
//                c.info(c.enclosingPosition, show(result), true)
                result
              } catch {
                case NonFatal(e) =>
                  e.printStackTrace()
                  throw e
              }
          })
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
                delayType(new DelayTreeCreator {
                  override def apply(c: whitebox.Context): c.universe.Tree =
                    try {
                      import c.universe._

                      val lowerBounds = members.collect(scala.Function.unlift { member =>
                        val memberSymbol = member.asInstanceOf[Symbol]
                        val narrowSuperType = internal.superType(internal.thisType(c.internal.enclosingOwner),
                                                                 internal.thisType(memberSymbol.owner))
                        val TypeBounds(_, lowerBound) = memberSymbol.infoIn(narrowSuperType)
                        if (lowerBound =:= definitions.AnyTpe) {
                          None
                        } else {
                          Some(tq"$lowerBound")
                        }
                      })
                      val compoundTypeTree = CompoundTypeTree(Template(lowerBounds.toList, noSelfType, Nil))
                      val result = q"override type ${TypeName(name)} = $compoundTypeTree"
//                      c.info(c.enclosingPosition, show(result), true)
                      result
                    } catch {
                      case NonFatal(e) =>
                        e.printStackTrace()
                        throw e
                    }
                })
            }
        val superTypes: Stream[c.universe.Type] = demixin(mixinType)

        val superTrees = for (superType <- superTypes) yield {
          tq"$superType"
        }
        val result = q"""
        new _root_.com.thoughtworks.Override[$valsType, $mixinType](_ match { case $pattern =>
          new ..$superTrees {
            ..$upvalues
            ..$overridenTypes
            ..$injects
          }
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
