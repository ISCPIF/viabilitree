package fr.iscpif.consumer

import fr.iscpif.viability.differential._
import fr.iscpif.kdtree.structure._

object Consumer {

  val integrationStep = 0.002
  val timeStep = 0.2

  def apply(state: Point, control: Point) = {
    def xDot(state: Array[Double], t: Double) = state(0) - state(1)
    def yDot(state: Array[Double], t: Double) = control(0)
    val dynamic = Dynamic(xDot, yDot)
    val res = dynamic.integrate(state.toArray, integrationStep, Seq(0.0, timeStep)).last._2.toSeq
    res.toSeq
  }

}

