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

import fr.iscpif.model.Control
import fr.iscpif.viability._
import fr.iscpif.kdtree.algorithm._
import fr.iscpif.kdtree.structure._
import fr.iscpif.kdtree.content._
import fr.iscpif.kdtree.visualisation._
import scala.util.Random
import scalax.io.Resource
import fr.iscpif.viability._
import kernel._
import control._

object ConsumerViability extends App
    with ViabilityKernel
    with ZoneInput
    with ZoneK
    with ParallelEvaluator
    with GridSampler {

  override def dilations = 0

  def controls = (-0.5 to 0.5 by 0.1).map(Control(_))

  def zone = Seq((0.0, 2.0), (0.0, 3.0))

  def depth = 16

  def dynamic(point: Point, control: Point) = Consumer(point, control)

  def dimension = 2

  def initialZone = zone

  implicit lazy val rng = new Random(42)

  val it = apply

  for {
    (b, s) <- it.zipWithIndex
    if s % 10 == 0 || !it.hasNext
  } {
    println(s)
    b.saveVTK2D(Resource.fromFile(s"/tmp/consumer/consumerGRID${depth}s$s.vtk"))
  }

}
