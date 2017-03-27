# Constructor.scala <a href="http://thoughtworks.com/"><img align="right" src="https://www.thoughtworks.com/imgs/tw-logo.png" title="ThoughtWorks" height="15"/></a>

[![Build Status](https://travis-ci.org/ThoughtWorksInc/Constructor.scala.svg?branch=master)](https://travis-ci.org/ThoughtWorksInc/Constructor.scala)
[![Latest version](https://index.scala-lang.org/thoughtworksinc/constructor.scala/constructor/latest.svg)](https://index.scala-lang.org/thoughtworksinc/constructor.scala/constructor)
[![Scaladoc](https://javadoc.io/badge/com.thoughtworks.constructor/constructor_2.12.svg?label=scaladoc)](https://javadoc.io/page/com.thoughtworks.constructor/constructor_2.12/latest/com/thoughtworks/index.html)


**Constructor.scala** is a library for dynamically creating classes and traits, especially dynamic mixins.

## Usage

``` sbt
// Add this line into your build.sbt
libraryDependencies += "com.thoughtworks.constructor" %% "constructor" % "latest.release"
```

``` scala
trait A
trait B

def makeAWithB()(implicit constructor: Constructor[() => A with B]): A with B = {
  constructor.newInstance()
}

val ab: A with B = makeAWithB()
```

## Motivation

This feature is useful for library authors.
A library author may ask his user to create a `trait` type, then dynamically mix-in it with the features provided by the library.

Suppose you are creating a [DSL](https://martinfowler.com/bliki/DomainSpecificLanguage.html) that compiles to JavaScript.

You want your DSL is extensible.
For example, the DSL users should be able to create custom binary operators.

With the help of `Constructor.scala`, you can put the boilerplate code into a private class `BinaryOperator`:

``` scala
trait Ast

object Ast {

  class Literal(val n: Int) extends Ast {
    override final def compile(): String = n.compile()
  }

  private[Ast] abstract class BinaryOperator(leftHandSide: Ast, rightHandSide: Ast) extends Ast {
    protected def symbol: String
    override final def compile() = s"($leftHandSide $symbol $rightHandSide)"
  }

  def binaryOperator[T](leftHandSide: Ast, rightHandSide: Ast)(
    implicit constructor: Constructor[(Ast, Ast) => BinaryOperator with T]): BinaryOperator with T = {
    constructor.newInstance(leftHandSide, rightHandSide)
  }

}
```

The users of only need a very simple implementation for their custom binary operators.

``` scala
import Ast._

trait Plus {
  protected final def symbol = "+"
}

trait Minus {
  protected final def symbol = "-"
}

val myAst = binaryOperator[Plus](
  new Literal(1),
  binaryOperator[Minus](
    new Literal(3),
    new Literal(5)
  )
)

print(myAst.compile()) // Output: "(1 + (3 - 5))"
```

## An alternative approach

There is another approach to integrate partial implementation from users: asking users to provide custom callback functions or type classes.

However, the callback functions or type classes approach will create additional object instances and additional references for each instance at run-time.
On the other hand, the `Constructor.scala` approach create classes at compile-time and no additional run-time references.
As a result, at run-time, `Constructor.scala` approach will consume less memory, and performs less indirect access on memory.

## Reducing number of generated classes

When you `Constructor` an abstract class or trait, an anonymous will be created. You can use [shapeless.Cached](https://static.javadoc.io/com.chuusai/shapeless_2.12/2.3.2/shapeless/Cached.html) along with `Constructor` to sharing generated classes.
