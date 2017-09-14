package viabilitree.approximation.example.lake

import viabilitree.model._
import scala.math._

/**
 * Created by ia on 11/07/2017.
 */
class Lake2(
  integrationStep: Double = 0.01,
  timeStep: Double = 0.1,
  b: Double = 0.8,
  r: Double = 1.0,
  m: Double = 1.0) {

  def dynamic(state: Vector[Double], control: Vector[Double]) = {
    def xDot(state: Vector[Double], t: Double) = control(0) + control(1)
    def yDot(state: Vector[Double], t: Double) =
      state(0) - (b * state(1) - r * math.pow(state(1), 8) / (1 + pow(state(1), 8)))
    val dynamic = Dynamic(xDot, yDot)
    dynamic.integrate(state.toArray, integrationStep, timeStep)
  }
}
