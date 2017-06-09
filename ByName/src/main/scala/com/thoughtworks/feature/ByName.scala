package com.thoughtworks.feature
import simulacrum.{op, typeclass}

import scala.language.higherKinds
import scala.language.implicitConversions

/** A type class for convert between by-name type and normal by-value type.
  *
  * This [[ByName]] type class provide an extension method [[extract]] for [[ByName.=>]] types via implicit view,
  * which can be import as follow:
  * {{{
  * import com.thoughtworks.feature.ByName.ops._
  * }}}
  *
  *
  * @tparam ByNameType The by-name type
  * @note When using this [[ByName]] type class,
  *       you should make sure the type parameter `ByNameType` is an abstract type,
  *       in case of [[https://github.com/scala/bug/issues?q=label%3Abyname Scala compiler bugs]].
  *
  *       You can use a trick similar to C++'s Pimpl Idiom to create opacity abstract types
  *
  *       {{{
  *       import com.thoughtworks.feature.ByName.`=>`
  *       trait OpacityTypes {
  *         type IntByName
  *         implicit def typeClass: ByName.Aux[IntByName, Int]
  *       }
  *
  *       val opacityTypes: OpacityTypes = new OpacityTypes {
  *         type IntByName = `=>`[Int]
  *         override def typeClass = ByName[IntByName]
  *       }
  *
  *       import opacityTypes._
  *       }}}
  *
  *       Now `IntByName` is an abstract type with underlying `=> Int` type.
  *
  *       <hr/>
  *
  *       Given a by-name parameter of the return value of `getInt()`,
  *
  *       {{{
  *       val getInt = stubFunction[Int]
  *       val boxedByName: IntByName = typeClass.make(getInt())
  *       }}}
  *
  *       when `getInt` returns 42,
  *
  *       {{{
  *       getInt.when.once returns 42
  *       }}}
  *
  *       then the value of the the by-name parameter should be 42;
  *
  *       {{{
  *       boxedByName.extract should be(42)
  *       }}}
  *
  *       when `getInt` returns 144,
  *
  *       {{{
  *       getInt.when.once returns 144
  *       }}}
  *
  *       then the value of the the by-name parameter should be 144.
  *
  *       {{{
  *       boxedByName.extract should be(144)
  *       }}}
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
@typeclass
trait ByName[ByNameType] extends AnyRef {
  type Value

  /** Creates a by-name parameter. */
  def make(byName: => Value): ByNameType

  // Disable Scaladoc due to https://github.com/mpilquist/simulacrum/issues/82
  // /** Returns the value of the by-name parameter `byName`. */
  def extract(byName: ByNameType): Value

}

object ByName {
  type Aux[Boxed, Value0] = ByName[Boxed] {
    type Value = Value0
  }

  implicit def ofValue[Value0]: ByName.Aux[`=>`[Value0], Value0] = {
    trait Make[A] {
      def make(a: A): A = a
    }

    new ByName[`=>`[Value0]] with Make[`=>`[Value0]] {
      override type Value = Value0

      override def extract(byName: => Value0): Value0 = byName
    }
  }

  private[ByName] val byNameContainer = {
    def unwrap[ByName0[_]](f: ByName0[Unit] => Unit) = new {
      type `=>`[Value] = ByName0[Value]
    }
    val f: (=> Unit) => Unit = { _ =>
      ()
    }
    unwrap(f)
  }

  /** An alias to by-name type.
    *
    * @note Unlike by-name types created from keyword `=>`, this [[=>]] alias can be used for return types.
    *       {{{
    *       import com.thoughtworks.feature.ByName.`=>`
    *       "def keywordByName: => Int = ???" shouldNot compile
    *       "def boxedByName: `=>`[Int] = ???" should compile
    *       }}}
    *
    * @note Unlike by-name types created from keyword `=>`, this [[=>]] alias can be used for type parameters.
    *       {{{
    *       import com.thoughtworks.feature.ByName.`=>`
    *       "val keywordByNameList = List.empty[=> Int]" shouldNot compile
    *       "val boxedByNameList = List.empty[`=>`[Int]]" should compile
    *       }}}
    */
  type `=>`[Value] = byNameContainer.`=>`[Value]
}
