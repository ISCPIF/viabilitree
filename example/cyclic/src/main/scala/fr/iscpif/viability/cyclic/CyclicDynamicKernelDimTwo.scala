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

object CyclicDynamicKernelDimTwo extends App
    with ViabilityKernel
    with ZoneInput
    with ParallelEvaluator
    with RandomSampler {

  val time = 0.1

  def k(p: Point) =
    p(1) >= -0.5 && p(1) <= 0.5 &&
      p(0) >= -3 && p(0) <= 3

  def dynamic(p: Point) = CyclicDynamicTwo(p, time)

  def zone =
    Seq(
      (-4.0, 4.0),
      (-4.0, 4.0)
    )

  def depth = 20

  def dimension = 2

  override def dilations: Int = 1

  override def endOfStep(s: Int, t: Tree[CONTENT]) =
    t.saveVTK2D(Resource.fromFile(s"/tmp/cycle/cycle2Dd${depth}s$s.vtk"))

  implicit lazy val rng = new Random(42)

  val bassin = apply.get

  println(bassin.volume)
  bassin.saveVTK2D(Resource.fromFile("/tmp/cycle/cycle2D.vtk"))

}
