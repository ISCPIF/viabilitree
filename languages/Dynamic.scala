/*
 * Copyright (C) 2013 de Aldama / Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


package fr.iscpif.viability



import org.apache.commons.math3.ode.FirstOrderDifferentialEquations
import org.apache.commons.math3.ode.nonstiff.ClassicalRungeKuttaIntegrator

class Dynamic(equations: (Array[Double], Double) => Double*) extends FirstOrderDifferentialEquations {
  def integrate(y0: Array[Double], timeStep: Double, samples: Seq[Double]): List[(Double, Array[Double])] = {
    val integrator = new ClassicalRungeKuttaIntegrator(timeStep)

    samples.tail.foldLeft((samples.head, y0) :: Nil) {
      case(ys, s) => {
        val (curT, curY) = ys.head
        val y = Array.ofDim[Double](equations.size)
        integrator.integrate(this, curT, curY, s, y)
        (s, y) :: ys
      }
    }.reverse
  }

  override def computeDerivatives(t: Double, y: Array[Double], yDot: Array[Double]): Unit =
    equations.zipWithIndex.foreach {case(eq, i) => yDot(i) = eq(y, t)}

  override def getDimension: Int = equations.size




}