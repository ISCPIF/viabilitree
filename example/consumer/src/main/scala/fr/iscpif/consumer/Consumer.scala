package fr.iscpif.consumer

import fr.iscpif.model._
import fr.iscpif.kdtree.structure._
import viabilitree.model.Dynamic

object Consumer {

  val integrationStep = 0.002
  val timeStep = 0.1

  def apply(state: Point, control: Point) = {
    def xDot(state: Array[Double], t: Double) = state(0) - state(1)
    def yDot(state: Array[Double], t: Double) = control(0)
    val dynamic = Dynamic(xDot, yDot)
    dynamic.integrate(state.toArray, integrationStep, timeStep)
  }

}

