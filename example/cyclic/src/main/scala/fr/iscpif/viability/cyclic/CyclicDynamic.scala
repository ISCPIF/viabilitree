package fr.iscpif.viability.cyclic

import fr.iscpif.viability.differential._

object CyclicDynamic {

  val timeStep = 0.0001

  def xDot(state: Array[Double], t: Double) = -state(1)
  def yDot(state: Array[Double], t: Double) = state(0)
  def zDot(state: Array[Double], t: Double) = 0

  val dynamic = Dynamic(xDot, yDot, zDot)

  def apply(v: Seq[Double], time: Double) =
    dynamic.integrate(v.toArray, timeStep, Seq(0.0, time)).last._2.toSeq

}

