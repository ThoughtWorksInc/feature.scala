package com.thoughtworks.feature

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

final class Caller[+A](val value: A) extends AnyVal

object Caller {
  implicit def generate[A]: Caller[A] = macro thisCaller[A]

  def thisCaller[A](c: Context): c.Expr[Caller[A]] = {
    import c.universe._
    c.Expr[Caller[A]](q"new _root_.com.thoughtworks.feature.Caller[this.type](this)")
  }
}
