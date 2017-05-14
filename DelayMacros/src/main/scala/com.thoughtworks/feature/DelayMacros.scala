package com.thoughtworks.feature

import scala.language.experimental.macros

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
trait DelayMacros {
  val c: whitebox.Context
  import c.universe._
  import com.thoughtworks.feature.DelayMacros._

  final def delayType(creator: DelayTreeCreator): TypeDef = {
    val name = c.freshName()
    typeCreators.synchronized {
      typeCreators(name) = creator
    }
    q"@_root_.com.thoughtworks.feature.DelayMacros.delay type ${TypeName(name)}"
  }
  final def delayValOrDef(creator: DelayTreeCreator): DefDef = {
    val name = c.freshName()
    valOrDefCreators.synchronized {
      valOrDefCreators(name) = creator
    }
    q"@_root_.com.thoughtworks.feature.DelayMacros.delay def ${TermName(name)}"
  }

}

object DelayMacros {

  private val typeCreators = scala.collection.mutable.HashMap.empty[String, DelayTreeCreator]
  private val valOrDefCreators = scala.collection.mutable.HashMap.empty[String, DelayTreeCreator]

  final class delay extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro AnnotationMacros.macroTransform
  }

  private[DelayMacros] final class AnnotationMacros(val c: whitebox.Context) {
    import c.universe._
    def macroTransform(annottees: Tree*): Tree =
      try {
        annottees.head match {
          case TypeDef(_, TypeName(typeName), _, _) =>
            val Some(creator) = typeCreators.synchronized {
              typeCreators.remove(typeName)
            }
            creator(c)
          case DefDef(_, TermName(termName), _, _, _, _) =>
            val Some(creator) = typeCreators.synchronized {
              valOrDefCreators.remove(termName)
            }
            creator(c)
        }
      } catch {
        case NonFatal(e) =>
          c.info(c.enclosingPosition, show(e), true)
          throw e
      }
  }

  trait DelayTreeCreator {
    def apply(c: whitebox.Context): c.universe.Tree
  }

}
