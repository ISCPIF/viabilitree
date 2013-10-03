 /*
 * Copyright (C) 30/09/13 Isabelle Alvarez
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

package fr.iscpif.population

import fr.iscpif.viability._
import fr.iscpif.kdtree.algorithm._
import fr.iscpif.kdtree.structure._
import fr.iscpif.kdtree.content._
import fr.iscpif.kdtree.visualisation._
import scala.util.Random
import scalax.io.Resource

object PopulationViability extends App
    with ViabilityKernel
    with ZoneInput
    with ParallelEvaluator
    with GridSampler {
  val a = 0.2
  val b = 3.0
  val c = 0.5
  val d = -2.0
  val e = 2.0

  def k(p: Point): Boolean = p(0) >= a && p(0) <= b && p(1) <= e && p(1) >= d

  def depth: Int = 8

  def zone = Seq((a, b), (d, e))

  def dynamic(point: Point, control: Point) = Population(point, control)
  def controls = (-0.5 to 0.5 by 0.1).map(Seq(_))

  def dimension = 2

  def initialZone = zone

  implicit lazy val rng = new Random(42)

  for {
    (b, s) <- apply.zipWithIndex
  } {
    println(s)
    b.saveVTK2D(Resource.fromFile(s"/tmp/population/populationGRID${depth}s$s.vtk"))
  }

}
