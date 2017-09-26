package viabilitree.approximation.example.lake

import viabilitree.viability._
import viabilitree.viability.kernel._
import viabilitree.export._
import viabilitree.model._
import math._

/**
 * Created by ia on 11/07/2017.
 */
object TestControl extends App {
  val b = 0.1
  val r = 0.1

  def lake(state: Vector[Double], control: Vector[Double]) = {
    def xDot(state: Vector[Double], t: Double) = control(0) + control(1)
    def yDot(state: Vector[Double], t: Double) = state(0) - (b * state(1) - r * math.pow(state(1), 8) / (1 + pow(state(1), 8)))
    val dynamic = Dynamic(xDot, yDot)
    dynamic.integrate(state.toArray, 0.01, 0.1)
  }

  val rng = new util.Random(42)

  val vk = KernelComputation(
    dynamic = lake,
    depth = 4,
    zone = Vector((0.1, 1.0), (0.0, 1.4)),
    controls = Vector((0.0 to 0.0 by 0.1), (-0.1 to 0.01 by 0.01)))

  // Note NumericalRange n'accepte pas un step à zéro (0.0 to 0.0 by 0.0) NON mais (0.0 to 0.0 by 0.1) OUI

  val (ak, steps) = approximate(vk, rng)
  saveHyperRectangles(vk)(ak, s"/tmp/TestControlLakeD${vk.depth}.txt")
}
