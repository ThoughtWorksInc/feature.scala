package com.thoughtworks.feature

import scala.reflect.api.Universe
import com.thoughtworks.Extractor._

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
class Untyper[Universe <: Singleton with scala.reflect.api.Universe](val universe: Universe) {
  import universe._

  /** Returns the instance tree for given singleton type */
  def singletonValue: PartialFunction[Type, Tree] = {
    case ThisType(symbol) => q"$symbol.this"
    case SingleType(NoPrefix, sym) => q"${sym.name.toTermName}"
    case SingleType(singletonValue.extract(pre), sym) => q"$pre.$sym"
    case SuperType(singletonValue.extract(thisValue), ThisType(superSymbol)) =>
      Super(thisValue, superSymbol.name.toTypeName)
  }

  def termSymbol: PartialFunction[Symbol, TermSymbol] = {
    case symbol if symbol.isTerm => symbol.asTerm
  }

  def typeSymbol: PartialFunction[Symbol, TypeSymbol] = {
    case symbol if symbol.isType => symbol.asType
  }

  def typeDefinitionSymbol: PartialFunction[TypeSymbol, (TypeName, Seq[Symbol], Type)] = {
    case symbol if !symbol.isClass =>
      val info = symbol.info
      (symbol.name.toTypeName, info.typeParams, info.resultType)
  }

  def varDefinitionSymbol: PartialFunction[TermSymbol, (TermName, Type)] = {
    case symbol if symbol.isVar =>
      (symbol.name.toTermName, symbol.info.resultType)
  }
  def valDefinitionSymbol: PartialFunction[TermSymbol, (TermName, Type)] = {
    case symbol if symbol.isVal || symbol.isStable =>
      (symbol.name.toTermName, symbol.info.resultType)
  }

  def defDefinitionSymbol: PartialFunction[TermSymbol, (TermName, Seq[Symbol], Seq[Seq[Symbol]], Type)] = {
    case symbol if symbol.isMethod =>
      val info = symbol.info
      (symbol.name.toTermName, info.typeParams, info.paramLists, info.finalResultType)

  }

  def typeDefinition: PartialFunction[TypeSymbol, TypeDef] = {
    case typeSymbol.extract(
        typeDefinitionSymbol.extract(name,
                                     typeDefinition.extract.forall(params),
                                     TypeBounds(untype.extract(upper), untype.extract(lower)))) =>
      q"type $name[..$params] >: $upper <: $lower"
    case typeSymbol.extract(
        typeDefinitionSymbol
          .extract(name, typeDefinition.extract.forall(params: Seq[TypeDef]), untype.extract(concreteType))) =>
      q"type $name[..$params] = $concreteType"
  }

  def definition: PartialFunction[Symbol, Tree] = {
    case typeDefinition.extract(typeDef) => typeDef
    case termDefinition.extract(termDef) => termDef
  }

  def termDefinition: PartialFunction[Symbol, Tree] = {
    case termSymbol.extract(varDefinitionSymbol.extract(name, untype.extract(result))) =>
      q"var $name: $result"
    case termSymbol.extract(symbol @ valDefinitionSymbol.extract(name, untype.extract(result))) =>
      if (symbol.isImplicit) {
        q"implicit val $name: $result"
      } else {
        q"val $name: $result"
      }
    case termSymbol.extract(
        defDefinitionSymbol.extract(name,
                                    typeDefinition.extract.forall(typeParams),
                                    termDefinition.extract.forall.forall(params),
                                    untype.extract(result))) =>
      q"def $name[..$typeParams](...$params): $result"
  }

  def untype: PartialFunction[Type, Tree] = {
    case ConstantType(value) =>
      Literal(value)
    case singletonValue.extract(value) =>
      tq"$value.type"
    case TypeRef(NoPrefix, sym, args) =>
      tq"${sym.name.toTypeName}[..${args.map(untype)}]"
    case TypeRef(singletonValue.extract(pre), sym, args) =>
      tq"$pre.$sym[..${args.map(untype)}]"
    case RefinedType(untype.extract.forall(parents), decls) =>
      CompoundTypeTree(Template(parents.toList, noSelfType, decls.view.map(definition).toList))
    case PolyType(typeSymbol.extract.forall(typeDefinition.extract.forall(typeParams)), untype.extract(resultType)) =>
      tq"({type Λ$$[..$typeParams] = $resultType})#Λ$$"
    case ExistentialType(definition.extract.forall(quantified), untype.extract(underlying)) =>
      tq"$underlying forSome { ..$quantified }"
    case AnnotatedType(annotations, untype.extract(underlying)) =>
      annotations.foldLeft(underlying) { (tree, annotation) =>
        Annotated(annotation.tree, tree)
      }
  }

}
