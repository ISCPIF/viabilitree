/*
 * Copyright (C) 11/09/13 Romain Reuillon
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

package fr.iscpif.consumer

import fr.iscpif.viability._
import fr.iscpif.kdtree.algorithm._
import fr.iscpif.kdtree.structure._
import fr.iscpif.kdtree.content._
import fr.iscpif.kdtree.visualisation._
import scala.util.Random
import scalax.io.Resource
import math._

object ConsumerKernel extends App with OracleApproximation with ZoneAndPointInput {
  def oracle(p: Point) = {
    p(1) <= p(0) + 0.5 * (1 - exp((p(0) - 2) / 0.5)) &&
      p(1) >= p(0) - 0.5 * (1 - exp(-p(0) / 0.5))
  }

  def zone = Seq((0.0, 2.0), (0.0, 3.0))

  def point = Seq(0.001, 0.001)

  def depth = 10

  apply.get.saveVTK2D(Resource.fromFile(s"/tmp/consumer/kernel.vtk"))
}
