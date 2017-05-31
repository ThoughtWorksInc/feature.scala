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

  trait ToFunction[ByNameParameter] {
    type Result
    def apply(p: ByNameParameter): () => Result
  }

  object ToFunction {

    type Aux[ByNameParameter, Result0] = ToFunction[ByNameParameter] {
      type Result = Result0
    }

    def apply[ByNameParameter](
        implicit toFunction: ToFunction[ByNameParameter]): ToFunction.Aux[ByNameParameter, toFunction.Result] =
      toFunction

    implicit def materialize[Result0] = {
      def apply0(byNameParameter: => Result0): () => Result0 = byNameParameter _

      def makeTypeClass[ByNameParameter](apply0: ByNameParameter => (() => Result0)) =
        new ToFunction[ByNameParameter] {
          override type Result = Result0
          def apply(p: ByNameParameter): () => Result0 = apply0(p)
        }

      makeTypeClass(apply0 _)
    }

  }

  trait FromFunction[Result] {
    type ByNameParameter
    def apply(p: () => Result): ByNameParameter

  }
  object FromFunction {

    type Aux[Result, ByNameParameter0] = FromFunction[Result] {
      type ByNameParameter = ByNameParameter0
    }

    implicit def apply[Result] = {
      import scala.language.higherKinds
      trait ~>[F[_], G[_]] {
        def apply[A](fa: F[A]): G[A]
      }

      def cps[A](f: λ[α => () => Result] ~> λ[α => (((A => α)) => α)]): FromFunction.Aux[Result, A] = {
        new FromFunction[Result] {
          type ByNameParameter = A
          def apply(p: () => Result): ByNameParameter = f[A](p)(identity)
        }
      }

      cps(new (λ[α => () => Result] ~> λ[α => (((=> Result) => α) => α)]) {
        def apply[A](fa: () => Result): ((=> Result) => A) => A = { (callback: ((=> Result) => A)) =>
          callback(fa()): A
        }
      })

    }

  }

}
