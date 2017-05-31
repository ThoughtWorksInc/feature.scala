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

}
