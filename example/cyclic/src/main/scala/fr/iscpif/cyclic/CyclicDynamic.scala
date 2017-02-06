package fr.iscpif.cyclic

import fr.iscpif.model._
import fr.iscpif.kdtree.structure._
import viabilitree.model.Dynamic

object CyclicDynamic {

  val integrationStep = 0.00001
  val timeStep = 0.0001

  def apply(state: Point, control: Point) = {
    def xDot(state: Array[Double], t: Double) = -state(1)
    def yDot(state: Array[Double], t: Double) = state(0)
    def zDot(state: Array[Double], t: Double) = 0
    val dynamic = Dynamic(xDot, yDot, zDot)
    val res = dynamic.integrate(state.toArray, integrationStep, Seq(0.0, timeStep)).last._2.toSeq
    res.toSeq
  }

}

