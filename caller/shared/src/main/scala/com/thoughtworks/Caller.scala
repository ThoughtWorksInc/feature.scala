package com.thoughtworks

import scala.language.experimental.macros
import scala.reflect.macros.Context

final case class Caller[+A](value: A)

object Caller {
  implicit def generate: Caller[Any] = macro thisCaller

  def thisCaller(c: Context) = {
    import c.universe._
    c.Expr[Caller[Any]](q"new _root_.com.thoughtworks.Caller[this.type](this)")
  }
}
