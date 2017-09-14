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

import viabilitree.approximation._
import viabilitree.export._
import math._
import scala.util.Random

object ConsumerKernel extends App {

  val b = 2.0
  val e = 3.0
  val c = 0.5

  def oracle(p: Vector[Double]) =
    (p(1) <= b && p(0) >= p(1) - c + (c * exp(-p(1) / c)) &&
      p(0) <= p(1) + c - (c * exp((p(1) - b) / c))) ||
      (p(1) >= b && (p(0) >= p(1) - c + (c * exp(-p(1) / c))))

  val approximation =
    OracleApproximation(
      depth = 20,
      box = Vector((0.0, b), (0.0, e)),
      oracle = oracle,
      point = Some(Vector(0.001, 0.001)))

  val rng = new Random(42)

  val res = approximate(approximation)(rng).get
  val leClean = clean(res)
  println(volume(res))
  println(volume(leClean))

  saveVTK2D(res, s"/tmp/testConsumer${approximation.depth}.vtk")
  saveVTK2D(leClean, s"/tmp/testConsumerCleanD${approximation.depth}.vtk")
  saveHyperRectangles(approximation)(res, s"/tmp/testConsumerPointD${approximation.depth}.txt")
  saveHyperRectangles(approximation)(leClean, s"/tmp/testConsumerCleanPointD${approximation.depth}.txt")

  //  def zone = Seq((0.0, b), (0.0, e))
  //
  //  def point = Seq(0.001, 0.001)
  //
  //  def depth = 16
  //
  //  val kernel = apply.get
  //
  //  saveVTK2D(kernel, s"/tmp/consumer/kernelV${depth}.vtk")
  //  saveVTK2D(dilate(dilate(kernel)), s"/tmp/consumer/kernel_dilatedV${depth}.vtk")

}
