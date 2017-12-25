package com.thoughtworks.feature

import scala.reflect.api.Universe
import com.thoughtworks.Extractor._

/** A utility to convert [[scala.reflect.api.Universe.Type Type]] to [[scala.reflect.api.Universe.Tree Tree]].
  *
  * @note The primary intent of '''Untyper''' class is for some macro libraries in this [[https://github.com/ThoughtWorksInc/feature.scala feature.scala]] project,
  *       although it may also useful for other projects.
  *
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
class Untyper[Universe <: Singleton with scala.reflect.api.Universe](val universe: Universe) {
  import universe._

  /** Returns the instance tree for given singleton type */
  def singletonValue: PartialFunction[Type, Tree] = {
    case ThisType(symbol) =>
      q"$symbol.this"
    case SingleType(NoPrefix, sym) =>
      q"${sym.name.toTermName}"
    case SingleType(pre, sym) if pre.typeSymbol.isPackage =>
      q"$sym"
    case SingleType(singletonValue.extract(pre), sym) =>
      q"$pre.$sym"
    case SuperType(singletonValue.extract(thisValue), ThisType(superSymbol)) =>
      Super(thisValue, superSymbol.name.toTypeName)
    case SingleType(untypeOption.extract(pre), sym) =>
      SelectFromTypeTree(pre, sym.name.toTypeName)
  }

  def termSymbol: PartialFunction[Symbol, TermSymbol] = {
    case symbol if symbol.isTerm => symbol.asTerm
  }

  def typeSymbol: PartialFunction[Symbol, TypeSymbol] = {
    case symbol if symbol.isType => symbol.asType
  }

  def typeDefinitionSymbol(implicit tpe: Type): PartialFunction[TypeSymbol, (TypeName, Seq[Symbol], Type)] = {
    case symbol if !symbol.isClass =>
      val info = symbol.infoIn(tpe)
      (symbol.name.toTypeName, info.typeParams, info.resultType)
  }

  def varDefinitionSymbol(implicit tpe: Type): PartialFunction[TermSymbol, (TermName, Type)] = {
    case symbol if symbol.isVar =>
      (symbol.name.toTermName, symbol.infoIn(tpe).resultType)
  }
  def valDefinitionSymbol(implicit tpe: Type): PartialFunction[TermSymbol, (TermName, Type)] = {
    case symbol if symbol.isVal || symbol.isStable =>
      (symbol.name.toTermName, symbol.infoIn(tpe).resultType)
  }

  def defDefinitionSymbol(
      implicit tpe: Type): PartialFunction[TermSymbol, (TermName, Seq[Symbol], Seq[Seq[Symbol]], Type)] = {
    case symbol if symbol.isMethod =>
      val info = symbol.infoIn(tpe)
      (symbol.name.toTermName, info.typeParams, info.paramLists, info.finalResultType)

  }

  def typeDefinition(implicit tpe: Type): PartialFunction[TypeSymbol, TypeDef] = {
    case typeSymbol.extract(
        typeDefinitionSymbol.extract(name,
                                     typeDefinition.extract.forall(params),
                                     TypeBounds(untypeOption.extract(upper), untypeOption.extract(lower)))) =>
      q"type $name[..$params] >: $upper <: $lower"
    case typeSymbol.extract(
        typeDefinitionSymbol
          .extract(name, typeDefinition.extract.forall(params: Seq[TypeDef]), untypeOption.extract(concreteType))) =>
      q"type $name[..$params] = $concreteType"
  }

  def definition(implicit tpe: Type): PartialFunction[Symbol, Tree] = {
    case typeDefinition.extract(typeDef) => typeDef
    case termDefinition.extract(termDef) => termDef
  }

  def termDefinition(implicit tpe: Type): PartialFunction[Symbol, Tree] = {
    case termSymbol.extract(varDefinitionSymbol.extract(name, untypeOption.extract(result))) =>
      q"var $name: $result"
    case termSymbol.extract(symbol @ valDefinitionSymbol.extract(name, untypeOption.extract(result))) =>
      if (symbol.isImplicit) {
        q"implicit val $name: $result"
      } else {
        q"val $name: $result"
      }
    case termSymbol.extract(
        defDefinitionSymbol.extract(name,
                                    typeDefinition.extract.forall(typeParams),
                                    termDefinition.extract.forall.forall(params),
                                    untypeOption.extract(result))) =>
      q"def $name[..$typeParams](...$params): $result"
  }

  def untypeOption: Type => Option[Tree] = { implicit tpe: Type =>
    tpe match {
      case ConstantType(value) =>
        Some(Literal(value))
      case singletonValue.extract(value) =>
        Some(tq"$value.type")
      case TypeRef(NoPrefix, sym, args) =>
        Some(tq"${sym.name.toTypeName}[..${args.map(untype)}]")
      case TypeRef(pre, sym, args) if pre.typeSymbol.isPackage =>
        Some(tq"$sym[..${args.map(untype)}]")
      case TypeRef(singletonValue.extract(pre), sym, args) =>
        Some(tq"$pre.$sym[..${args.map(untype)}]")
      case TypeRef(untypeOption.extract(pre), sym, args) =>
        Some(tq"$pre#$sym[..${args.map(untype)}]")
      case RefinedType(untypeOption.extract.forall(parents), decls) =>
        Some(CompoundTypeTree(Template(parents.toList, noSelfType, decls.view.map(definition).toList)))
      case PolyType(typeSymbol.extract.forall(typeDefinition.extract.forall(typeParams)), untypeOption.extract(resultType)) =>
        Some(tq"({type Λ$$[..$typeParams] = $resultType})#Λ$$")
      case ExistentialType(definition.extract.forall(quantified), untypeOption.extract(underlying)) =>
        Some(tq"$underlying forSome { ..$quantified }")
      case AnnotatedType(annotations, untypeOption.extract(underlying)) =>
        Some(annotations.foldLeft(underlying) { (tree, annotation) =>
          Annotated(annotation.tree, tree)
        })
      case _ =>
        None
    }

  }

  def untype: PartialFunction[Type, Tree] = scala.Function.unlift(untypeOption)

}
