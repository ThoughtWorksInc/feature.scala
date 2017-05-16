package com.thoughtworks.feature

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/** An implicit value that points to the function caller.
  * 
  * == Usage ==
  * {{{
  * libraryDependencies += "com.thoughtworks.feature" %% "caller" % "latest.release"
  * }}}
  *
  * === Getting the caller for logging or something ===
  * {{{
  * object Foo{
  *   def log()(implicit caller: Caller[Any]) = {
  *     println(caller.value)
  *   }
  * }
  * object Bar{
  *   Foo.log() // Bar
  * }
  * }}}
  * 
  * === Restricting who you can be called from ===
  * {{{
  * class IKnowWhatImDoing
  * object Foo{
  *   def runDangerous()(implicit caller: Caller[IKnowWhatImDoing]) = {
  *     println(caller.value)
  *   }
  * }
  * object Bar {
  *   Foo.runDangerous() // compile error
  * }
  * object Bar2 extends IKnowWhatImDoing{
  *   Foo.runDangerous() // ok, prints Bar2
  * }
  * }}}
  *
  * === Getting calling class or classloader, e.g. for loading resources, without needing to worry about properly setting up and tearing down the Context ClassLoader ===
  * {{{
  * object Foo{
  *   def getResource(path: String)(implicit caller: Caller[_]) = {
  *     caller.value.getClass.getClassLoader.getResourceAsStream(path)
  *   }
  * }
  * object Bar{
  *   Foo.getResource("/thing/file.txt") // loads resource from `Bar`s classloader, always
  * }
  * }}}
  * 
  * @param value The caller instance
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  * @author Li Haoyi
  */
final class Caller[+A](val value: A) extends AnyVal

object Caller {
  implicit def generate[A]: Caller[A] = macro thisCaller[A]

  def thisCaller[A](c: Context): c.Expr[Caller[A]] = {
    import c.universe._
    c.Expr[Caller[A]](q"new _root_.com.thoughtworks.feature.Caller[this.type](this)")
  }
}
