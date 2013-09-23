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

object ConsumerViability extends App
    with ViabilityKernel
    with ZoneInput
    with ParallelEvaluator
    with GridSampler
    with ConstantControlHeuristic {

  def controls = (-0.5 to 0.5 by 0.1).map(Seq(_))

  def k(p: Point) = p(0)>=0 && p(0) <= 2 && p(1) <= 3  && p(1)>=0

  def zone = Seq((0.0, 2.0), (0.0, 3.0))

  def depth = 16

  def dynamic(point: Point, control: Point) = Consumer(point, control)

  def dimension = 2

  def initialZone = zone


  implicit lazy val rng = new Random(42)

  /*controls.map {
    c =>
      println(c)
      Consumer(Seq(1.95, 1.85), c)
  }.foreach(println)   */



  for {
    (b, s) <- apply.zipWithIndex
  } b.saveVTK2D(Resource.fromFile(s"/tmp/consumer/consumerGRID${depth}s$s.vtk"))

  //println(bassin.volume)

}
