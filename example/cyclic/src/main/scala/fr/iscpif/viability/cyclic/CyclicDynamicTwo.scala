package fr.iscpif.viability.cyclic

import fr.iscpif.viability.differential._

object CyclicDynamicTwo {

  def dim = 2
  val timeStep = 0.0001

  def xDot(state: Array[Double], t: Double) = -state(1)
  def yDot(state: Array[Double], t: Double) = state(0)

  val dynamic = Dynamic(xDot, yDot)

  def apply(v: Seq[Double], time: Double) =
    dynamic.integrate(v.toArray, timeStep, Seq(0.0, time)).last._2.toSeq

}

