package fr.iscpif.lake

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

import fr.iscpif.model._
import fr.iscpif.kdtree.structure._
import math._
import scalax.io.Output

trait Lake {
  val integrationStep = 0.01
  val timeStep = 0.1
  val b = 0.8
  val r = 1.0
  val m = 1.0


  def dynamic(state: Point, control: Point) = {
    def xDot(state: Array[Double], t: Double) = control(0)
    // TODO to avoid unnecessary approximation when m=1
    // def yDot(state: Array[Double], t: Double) = b*state(1)-r*math.pow(state(1),8)/(pow(m,8)+pow(state(1),8))
    def yDot(state: Array[Double], t: Double) = state(0) - (b * state(1) - r * math.pow(state(1), 8) / (1 + pow(state(1), 8)))
    val dynamic = Dynamic(xDot, yDot)
    val res = dynamic.integrate(state.toArray, integrationStep, Seq(0.0, timeStep)).last._2.toSeq
    res.toSeq
  }


}