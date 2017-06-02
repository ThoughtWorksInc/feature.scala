package com.thoughtworks.feature

import java.io.{CharArrayWriter, InputStreamReader}
import java.net.URL

import org.apache.commons.io.IOUtils

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox
import scala.language.experimental.macros

/** An annotation to include code snippets into Scala source file.
  * == Sbt configuration ==
  * `<pre>
  * libraryDependencies += "com.thoughtworks.feature" %% "constructor" % "latest.release"
  *
  * libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided
  *
  * addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch)
  * </pre>`
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  *
  * @example Given [[https://gist.github.com/Atry/5dcb1414b804fd7679393cacac3c89fc/raw/5b1748ab6b45c00be0109686fdb25e85cde11ce0/include-example.sc a URL]]
  *          that defines `val i = 42`, when including it into `Container`.
  *
  *          {{{
  *          @include("https://gist.github.com/Atry/5dcb1414b804fd7679393cacac3c89fc/raw/5b1748ab6b45c00be0109686fdb25e85cde11ce0/include-example.sc")
  *          object Container {
  *            def j = i + 1
  *          }
  *          }}}
  *
  *          Then `i` should be defined in `Container`.
  *
  *          {{{
  *          Container.i should be(42)
  *          }}}
  *
  *          And other variables inside Container`, like `j` should be able to reference `i`.
  *
  *          {{{
  *          Container.j should be(Container.i + 1)
  *          }}}
  *
  */
final class include(urls: String*) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro include.Macros.macroTransform
}

object include {
  private[include] final class Macros(val c: whitebox.Context) {
    import c.universe._
    def macroTransform(annottees: Tree*): Tree = {
      val q"new $annotationClass(..$urlTrees)" = c.prefix.tree
      val contents = urlTrees.flatMap {
        case Literal(Constant(urlString: String)) =>
          val writer = new CharArrayWriter()
          writer.append('{')
          val inputStream = new URL(urlString).openConnection().getInputStream
          try {
            IOUtils.copy(inputStream, writer, scala.io.Codec.UTF8.charSet)
          } finally {
            inputStream.close()
          }
          writer.append('\n')
          writer.append('}')
          val q"{..$content}" = c.parse(writer.toString)
          content
      }

      val (head +: tail) = annottees

      val transformedHead = atPos(head.pos) {
        head match {
          case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
            DefDef(mods, name, tparams, vparamss, tpt, q"..$contents; $rhs")
          case ValDef(mods, name, tpt, rhs) =>
            ValDef(mods, name, tpt, q"..$contents; $rhs")
          case ClassDef(mods, name, tparams, impl @ Template(parents, self, body)) =>
            ClassDef(mods, name, tparams, atPos(impl.pos) {
              Template(parents, self, contents ++ body)
            })
          case ModuleDef(mods, name, impl @ Template(parents, self, body)) =>
            ModuleDef(mods, name, atPos(impl.pos) {
              Template(parents, self, contents ++ body)
            })
        }
      }
      q"$transformedHead;..$tail"
    }
  }
}
