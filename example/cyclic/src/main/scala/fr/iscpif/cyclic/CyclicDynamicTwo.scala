package fr.iscpif.cyclic

import fr.iscpif.model._
import fr.iscpif.kdtree.structure._

object CyclicDynamicTwo {

  val timeStep = 0.1
  val integrationStep = 0.00001

  def apply(state: Point, control: Point) = {
    def xDot(state: Array[Double], t: Double) = -state(1)
    def yDot(state: Array[Double], t: Double) = state(0)
    val dynamic = Dynamic(xDot, yDot)
    val res = dynamic.integrate(state.toArray, integrationStep, Seq(0.0, timeStep)).last._2.toSeq
    res.toSeq
  }
}

