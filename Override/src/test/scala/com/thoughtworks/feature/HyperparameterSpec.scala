package com.thoughtworks.feature

import com.thoughtworks.feature.Override.inject
import org.scalatest.{FreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class HyperparameterSpec extends FreeSpec with Matchers {
  import com.thoughtworks.feature.HyperparameterSpec._
  "Hyperparameter for Float" in {
    val hyperparameters = Override.newInstance[Hyperparameter.Lift with Hyperparameter.FixedLearningRate](fixedLearningRate = 0.125f)
    val weight = hyperparameters.lift(0.4f)
    weight.data should be(0.4f)
    weight.backward(0.25f)
    weight.data should be(0.36875f)
  }

}

object HyperparameterSpec {

  object Hyperparameter {

    trait FixedLearningRate extends LearningRate {
      def fixedLearningRate: Float

      trait FixedLearningRateOptimizer extends LearningRateOptimizer {
        def learningRate: Float = fixedLearningRate
      }

      type Optimizer <: FixedLearningRateOptimizer

    }

    trait LearningRate extends Hyperparameter {

      trait LearningRateOptimizer extends BackwardOptimizer {
        def learningRate: Float

        abstract override def delta: Float = super.delta * learningRate
      }

      type Optimizer <: LearningRateOptimizer

    }

    trait Lift extends Hyperparameter {

      abstract class LiftedWeight(final var data: Float) extends BackwardWeight { this: Weight =>
      }

      @inject
      def weightConstructor: Constructor[Float => LiftedWeight with Weight]

      def lift(initialValue: Float) = weightConstructor.newInstance(initialValue)

    }
  }
  trait Hyperparameter {

    @inject
    def optimizerConstructor: Constructor[(Weight, Float) => Delta0 with Optimizer]

    trait BackwardWeight { this: Weight =>

      var data: Float

      def backward(delta: Float) = {
        data = optimizerConstructor.newInstance(this, delta).newData
      }

    }
    type Weight <: BackwardWeight

    abstract class Delta0(override final val weight: Weight, delta0: Float) extends BackwardOptimizer {
      def delta: Float = delta0
    }

    trait BackwardOptimizer {
      def weight: Weight
      def newData: Float = weight.data - delta

      def delta: Float
    }
    type Optimizer <: BackwardOptimizer
  }

}
