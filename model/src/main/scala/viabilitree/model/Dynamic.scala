/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package viabilitree.model

import org.apache.commons.math3.ode._
import org.apache.commons.math3.ode.nonstiff._

object Dynamic {
  def apply(equations: (Vector[Double], Double) => Double*): Dynamic = new Dynamic(equations: _*)
}

class Dynamic(equations: (Vector[Double], Double) => Double*) extends FirstOrderDifferentialEquations {

  def integrate(y0: Array[Double], integrationStep: Double, step: Double): Vector[Double] =
    integrate(y0, integrationStep, Vector(0.0, step)).last._2.toVector

  def integrate(y0: Array[Double], integrationStep: Double, samples: Vector[Double]) = {
    val integrator = new ClassicalRungeKuttaIntegrator(integrationStep)

    samples.tail.foldLeft((samples.head, y0) :: Nil) {
      case (ys, s) => {
        val (curT, curY) = ys.head
        val y = Array.ofDim[Double](equations.size)
        integrator.integrate(this, curT, curY, s, y)
        // FIXME use Try and propagate up the stack
        if (y.exists(_.isNaN)) throw new RuntimeException(s"""Dynamic from ${curY.toVector} using integration step ${integrationStep} produces NaN: ${y.toVector}"""")
        (s, y) :: ys
      }
    }.reverse

  }

  override def computeDerivatives(t: Double, y: Array[Double], yDot: Array[Double]): Unit =
    equations.zipWithIndex.foreach { case (eq, i) => yDot(i) = eq(y.toVector, t) }

  override def getDimension: Int = equations.size
}
