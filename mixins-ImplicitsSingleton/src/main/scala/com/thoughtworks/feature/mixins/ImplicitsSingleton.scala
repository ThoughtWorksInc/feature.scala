package com.thoughtworks.feature
package mixins

import Factory.inject

/** A mixin that creates the instance of [[implicits]].
  *
  * Any fields and methods in [[Implicits]] added by other plugins will be mixed-in and present in [[implicits]].
  *
  * @note This [[ImplicitsSingleton]] supposes to be created by [[Factory]]
  */
trait ImplicitsSingleton {
  type Implicits

  @inject
  protected val implicitsFactory: Factory[Implicits]

  @inject
  protected val implicitApplyImplicitsConstructor: ImplicitApply[implicitsFactory.Constructor]

  @inject
  protected def asImplicits: implicitApplyImplicitsConstructor.Out <:< Implicits

  @transient
  lazy val implicits: Implicits = {
    asImplicits(implicitApplyImplicitsConstructor(implicitsFactory.newInstance))
  }

}
