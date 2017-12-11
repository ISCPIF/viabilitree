package viabilitree.approximation.example.raz13

import viabilitree.model.{ Dynamic, Model }

import scala.math.log

/**
 * Created by scala on 09/10/17.
 */
class Raz13B extends Model {
  def integrationStep: Double = 0.01
  def timeStep: Double = 0.1
  var Tm: Double = 3.0
  var A2: Double = 0.1
  var b: Double = 1.0
  var C: Double = 2.0
  var A3: Double = 1.0
  var M: Double = 5.0
  var a3: Double = 2.0
  var a2: Double = 0.0
  var a1: Double = 0.0
  var a0: Double = 1.0
  var v_m: Double = 0.8
  def vMax = 5.0
  def A1 = log(2) / Tm

  def dynamic(state: Vector[Double], control: Vector[Double]) = {
    def alphaDot(state: Vector[Double], t: Double) =
      -A1 * state(0) + A2 * state(0) * (1 - state(0)) * control(0)
    def wDot(state: Vector[Double], t: Double) = b - C * control(0)

    val dynamic = Dynamic(alphaDot, wDot)
    dynamic.integrate(state.toArray, integrationStep, timeStep)
  }

}
