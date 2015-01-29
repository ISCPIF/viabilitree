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

package fr.iscpif.lake

import fr.iscpif.kdtree.algorithm._
import fr.iscpif.kdtree.structure._
import fr.iscpif.kdtree.export._
import fr.iscpif.kdtree.content._
import fr.iscpif.kdtree._
import fr.iscpif.model.Control
import scala.util.Random
import scalax.io.Resource
import fr.iscpif.viability._
import kernel._
import control._
import scalax.io._

object LakeViabilityKernel extends App {

  val lake = new LakeViability with ZoneK { }

  implicit lazy val rng = new Random(42)

  for {
    (b, s) <- lake().zipWithIndex
  } {
    println(s)
    b.saveVTK2D(Resource.fromFile(s"/tmp/lake${lake.depth}/mu${lake.dilations}s$s.vtk"))
  }

}

trait LakeViability <: ViabilityKernel
  with ZoneInput
  with GridSampler
  // with ParallelEvaluator
  with Lake {
  override def dilations = 0
  def controls = (-0.09 to 0.09 by 0.01).map(Control(_))
  def zone = Seq((0.1, 1.0), (0.0, 1.4))
  def depth = 16
  def dimension = 2

  }

