/*
 * Copyright (C) 08/07/13 Romain Reuillon
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

package fr.iscpif.viability.cyclic

import fr.iscpif.viability._
import fr.iscpif.kdtree.algorithm._
import fr.iscpif.kdtree.structure._
import scala.util.Random
import fr.iscpif.kdtree.visualisation._
import scalax.io._

object CyclicDynamicKernel extends App
    with ViabilityKernel
    with ZoneInput
    with ParallelEvaluator
    with RandomSampler {

  val time = 0.1

  def k(p: Point) =
    p(2) >= 0 && p(2) <= 1 &&
      p(1) >= 0 && p(1) <= 1 &&
      p(0) >= -3 && p(0) <= 3

  def dynamic(p: Point) = CyclicDynamic(p, time)

  def zone =
    Seq(
      (-4.0, 4.0),
      (-4.0, 4.0),
      (-4.0, 4.0)
    )

  def depth = 21

  def dimension = 3

  override def endOfStep(s: Int, t: Tree[CONTENT]) =
    t.saveVTK3D(Resource.fromFile(s"/tmp/cycle$s.vtk"))

  implicit lazy val rng = new Random(42)

  val bassin = apply.get

  println(bassin.volume)
  bassin.saveVTK3D(Resource.fromFile("/tmp/cycle.vtk"))

}
