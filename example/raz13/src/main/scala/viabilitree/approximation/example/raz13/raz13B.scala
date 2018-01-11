package viabilitree.approximation.example.raz13

import viabilitree.model._
import math._

/**
 * Created by scala on 09/10/17.
 */
class Raz13B extends Model {
  var integrationStep: Double = 0.01
  var timeStep: Double = 0.1

  var Tm: Double = 2.0
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

  def d_1(alpha: Double, s: Double): Double = {
    a3 * s * s * s + a2 * s * s + a1 * s
  }

  def d_2(alpha: Double, s: Double): Double = {
    s >= v_m match {
      case false => 0.0
      case true => a0 * (s - v_m) * (s - v_m) * (s - v_m)
    }
  }

  def damage(alpha: Double, s: Double): Double = {
    (1 - alpha) * d_1(alpha, s) + alpha * d_2(alpha, s)
  }

  // a0*(1-alpha)*s

  def perturbation(state: Vector[Double], s: Double) = {
    def alphaDelta(state: Vector[Double], s: Double) = A3 * (1 - state(0)) * (s / (M + s))

    def wDelta(state: Vector[Double], s: Double) = -damage(state(0), s)

    (alphaDelta(state, s), wDelta(state, s))
  }

  def jump(state: Vector[Double], s: Double) = {
    val (alphaDelta, wDelta) = perturbation(state, s)
    Vector(state(0) + alphaDelta, state(1) + wDelta)
  }

  // pour avoir un vecteur en sortie
  /*
  def jump(state:Vector[Double], s: Double) = {
    val (alphaDelta,wDelta) = perturbation(state,s)
    (state(0) + alphaDelta, state(1) + wDelta )
  }
*/

  // On a besoin d'une soft_appartenance à un noyau qui tienne compte de la manière dont on sort de l'ensemble
  // TODO fix pb when some states are outside wLim (ex. v=3) outOfMemoryError

  def softJump(state: Vector[Double], jumpV: Vector[Double] => Vector[Double],
    viableSet: viabilitree.kdtree.Tree[viabilitree.viability.kernel.KernelContent],
    viabProblem: viabilitree.viability.kernel.KernelComputation): Boolean = {
    val jumpState = jumpV(state)
    val zoneLim = viabProblem.zone
    val wLim = zoneLim.region(1).max
    (viableSet.contains(viabilitree.viability.kernel.KernelContent.label.get, state) &&
      (viableSet.contains(viabilitree.viability.kernel.KernelContent.label.get, jumpState)) ||
      jumpState(1) >= wLim)

  }
}
