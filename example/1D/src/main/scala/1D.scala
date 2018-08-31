/*
 * Copyright (C) 20/07/2018 Isabelle Alvarez
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
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

package viabilitree.example.attractor1D

import viabilitree.model._
import scala.math._

case class Attractor1D(a: Double = 0.0, b: Double = -1.0, c: Double = 1.0, integrationStep: Double = 0.1, timeStep: Double = 1.0) {

  def dynamic(state: Vector[Double], control: Vector[Double]): Vector[Double] = {
    def x = state(0)
    def mu = control(0)

    def xDot(state: Vector[Double], t: Double) = (x-a)*(x-b)*(mu*x-c)

    val dynamic = Dynamic(xDot)
    dynamic.integrate(state.toArray, integrationStep, timeStep)
  }

}

