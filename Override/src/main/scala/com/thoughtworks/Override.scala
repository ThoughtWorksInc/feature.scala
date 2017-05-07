package com.thoughtworks
import macrocompat.bundle
import shapeless._
import scala.language.experimental.macros

import scala.reflect.macros.whitebox

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
trait Override[Vals, Result] extends RecordArgs {
  def newInstanceRecord(vals: Vals): Result
}

object Override {

  implicit final class FunctionOverride[Vals, Result](underlying: Vals => Result) extends Override[Vals, Result] {
    override def newInstanceRecord(vals: Vals): Result = underlying(vals)
  }

  def apply[Vals, Result](implicit constructor: Override[Vals, Result]): Override[Vals, Result] = constructor

  implicit def materialize[Vals, Result]: Override[Vals, Result] = macro Macros.materialize[Vals, Result]

  @bundle
  final class Macros(val c: whitebox.Context) extends CaseClassMacros with SingletonTypeUtils {
    import c.universe._

    def materialize[Vals: WeakTypeTag, Result: WeakTypeTag]: Tree = {
      val valsType = weakTypeOf[Vals]
      val pattern = unpackHListTpe(valsType).foldRight[Tree](pq"_root_.shapeless.HNil") { (field, accumulator) =>
        val FieldType(SingletonSymbolType(k), v) = field
        pq"_root_.shapeless.::(${TermName(k)}, $accumulator)"
      }
      val argumentHListName = TermName(c.freshName("argumentHList"))
      weakTypeOf[Result].dealias match {
        case mixinType @ RefinedType(superTypes, refinedScope) if refinedScope.isEmpty =>
          val superTrees = for (superType <- superTypes) yield {
            tq"$superType"
          }
          q"""
            { $argumentHListName: $valsType =>
              new ..$superTrees {
                override val $pattern = $argumentHListName
              }
            } : _root_.com.thoughtworks.Override[$valsType, $mixinType]
          """
        case classType =>
          q"""
            { $argumentHListName: $valsType =>
              new $classType {
                override val $pattern = $argumentHListName
              }
            } : _root_.com.thoughtworks.Override[$valsType, $classType]
          """
      }
    }
  }
}
