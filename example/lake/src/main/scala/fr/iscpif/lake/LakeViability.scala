package fr.iscpif.lake

import scala.App
import fr.iscpif.viability.{ConstraintSet, ViabilityKernel}
import fr.iscpif.kdtree.algorithm.{GridSampler, ParallelEvaluator, ZoneInput}
import fr.iscpif.kdtree.structure._
import fr.iscpif.kdtree.visualisation._
import scala.util.Random
import scalax.io.Resource

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

object LakeViability
  extends App
  with ViabilityKernel
  with ZoneInput
  with ParallelEvaluator
  with GridSampler {


  override def dilations = 0

    def controls = (-0.09 to 0.09 by 0.01).map(Seq(_))

    def zone = Seq((0.1, 1.0), (0.0, 1.4))

    def depth = 16

    def dynamic(point: Point, control: Point) = Lake(point, control)

    def dimension = 2

//    def initialZone = Seq((0, 1.5), (0.0, 2))

    implicit lazy val rng = new Random(42)

    for {
      (b, s) <- apply.zipWithIndex
    } {
      println(s)
      b.saveVTK2D(Resource.fromFile(s"/tmp/lake/Lake${depth}s$s.vtk"))
    }

}
