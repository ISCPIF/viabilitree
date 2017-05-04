/*
 * Copyright (C) 14/11/13 Romain Reuillon
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

package viabilitree.example.bilingual

import viabilitree.model._

import scala.math._

case class Bilingual(a: Double = 1.31, integrationStep: Double = 0.1, timeStep: Double = 1.0) {

  def dynamic(state: Vector[Double], control: Vector[Double]): Vector[Double] = {
    def sA = state(0)
    def sB = state(1)
    def s = state(2)
    def σaDot(state: Vector[Double], t: Double) =
      (1 - sA - sB) * pow(1 - sB, a) * s - sA * pow(sB, a) * (1 - s)

    def σbDot(state: Vector[Double], t: Double) =
      (1 - sA - sB) * pow(1 - sA, a) * (1 - s) - sB * pow(sA, a) * s

    def sDot(state: Vector[Double], t: Double) =
      control(0)

    val dynamic = Dynamic(σaDot, σbDot, sDot)
    dynamic.integrate(state.toArray, integrationStep, timeStep)
  }

}
