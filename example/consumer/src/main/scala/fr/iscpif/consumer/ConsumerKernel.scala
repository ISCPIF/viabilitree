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
import fr.iscpif.kdtree.export._
import scala.util.Random
import scalax.io.Resource
import math._

object ConsumerKernel extends App with OracleApproximation with ZoneAndPointInput {
  val b = 2.0
  val e = 3.0
  val c = 0.5

  def oracle(p: Point) = {
    (p(1) <= b && p(0) >= p(1) - c + (c * exp(-p(1) / c)) &&
      p(0) <= p(1) + c - (c * exp((p(1) - b) / c))) ||
      (p(1) >= b && (p(0) >= p(1) - c + (c * exp(-p(1) / c))))
  }

  def zone = Seq((0.0, b), (0.0, e))

  def point = Seq(0.001, 0.001)

  def depth = 16

  val kernel = apply.get

  saveVTK2D(kernel, Resource.fromFile(s"/tmp/consumer/kernelV${depth}.vtk"))
  saveVTK2D(dilate(dilate(kernel)), Resource.fromFile(s"/tmp/consumer/kernel_dilatedV${depth}.vtk"))

}
