package viabilitree.approximation.example.lake

/*
 * Copyright (C) 10/10/13 Isabelle Alvarez
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

import viabilitree.model._
import math._

case class Lake(
  integrationStep: Double = 0.01,
  timeStep: Double = 0.1,
  b: Double = 0.8,
  r: Double = 1.0,
  m: Double = 1.0) {

  def dynamic(state: Vector[Double], control: Vector[Double]) = {
    def xDot(state: Vector[Double], t: Double) = control(0)
    // TODO to avoid unnecessary approximation when m=1
    // def yDot(state: Array[Double], t: Double) = b*state(1)-r*math.pow(state(1),8)/(pow(m,8)+pow(state(1),8))
    def yDot(state: Vector[Double], t: Double) =
      state(0) - (b * state(1) - r * math.pow(state(1), 8) / (1 + pow(state(1), 8)))

    val dynamic = Dynamic(xDot, yDot)
    dynamic.integrate(state.toArray, integrationStep, timeStep)
  }

}
