package fr.iscpif.consumer

import viabilitree.model._

case class Consumer(
  integrationStep: Double = 0.002,
  timeStep: Double = 0.1) {

  def dynamic(state: Vector[Double], control: Vector[Double]) = {
    def xDot(state: Vector[Double], t: Double) = state(0) - state(1)
    def yDot(state: Vector[Double], t: Double) = control(0)
    val dynamic = Dynamic(xDot, yDot)
    dynamic.integrate(state.toArray, integrationStep, timeStep)
  }

}
