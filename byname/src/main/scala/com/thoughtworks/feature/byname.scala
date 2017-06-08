package com.thoughtworks.feature
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object byname {

  object ByName {
    def apply[A](a: => A): ByName[A] = {
      identity[ByName[A]](a)
    }
  }
  private[byname] val byNameContainer = {
    def unwrap[ByName0[_]](f: ByName0[Unit] => Unit) = new {
      type ByName[A] = ByName0[A]
    }
    val f: (=> Unit) => Unit = { _ =>
      ()
    }
    unwrap(f)
  }

  type ByName[A] = byNameContainer.ByName[A]

  trait IsByName[F] {
    type Value

    def make(byName: => Value): F
    def extract(byName: F): Value
  }

  object IsByName {
    type Aux[F, Value0] = IsByName[F] {
      type Value = Value0
    }
    implicit def apply[Value0]: IsByName.Aux[ByName[Value0], Value0] =
      new IsByName[ByName[Value0]] {
        override type Value = Value0

        override def make(byName: => Value0): ByName[Value0] = identity[ByName[Value0]](byName)

        override def extract(byName: => Value0): Value0 = byName
      }
  }
}
