/*
 * Copyright (C) 03/10/13 Isabelle Alvarez
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

package fr.iscpif.cyclic

import fr.iscpif.kdtree.algorithm._
import fr.iscpif.kdtree.structure._
import fr.iscpif.model.Control
import scala.util.Random
import scalax.io.Resource
import fr.iscpif.kdtree.export._
import fr.iscpif.viability._
import kernel._
import control._

object CyclicViability extends App
  with ViabilityKernel
  with ZoneInput
  with GridSampler
  with LearnK {

  override def dilations = 0

  def controls = Seq(Control(0.0))

  def k(p: Point) = p(0) >= -0.5 && p(0) <= 0.5 &&
    p(1) >= -0.5 && p(1) <= 0.5 &&
    p(2) >= -1 && p(2) <= 1

  def zone: Zone = Seq((-2.0, 2.0), (-2.0, 2.0), (-2.0, 2.0))

  def depth = 15

  def dynamic(point: Point, control: Point) = CyclicDynamic(point, control)

  def dimension = 3

  implicit lazy val rng = new Random(42)

  /*
  for {
    (b, s) <- apply.zipWithIndex
  } {
    println(s)
    b.saveVTK3D(Resource.fromFile(s"/tmp/cyclic/cyclicViab${depth}s$s.vtk"))
  }
*/

  val listeResult = apply.zipWithIndex
  listeResult.foreach {
    case (b, s) => {
      println("next step " + s)
      if (listeResult.hasNext && (s % 1 != 0)) println("on passe")
      else {
        saveVTK3D(b, s"/tmp/cyclic/cyclicViab${depth}s$s.vtk")
      }
    }
  }
}

