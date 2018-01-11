/*
 * Copyright (C) 14/11/13 Romain Reuillon
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

package viabilitree.example.bilingual

import viabilitree.export._
import viabilitree.viability._
import viabilitree.viability.basin._

import scala.Ordering.Implicits._
import scala.collection.immutable._
import scala.io._
import scala.util.Random

object BilingualBasin extends App {

  def fileContent =
    Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("targetData.txt")).getLines().map { l =>
      l.split(" ").take(3).map(_.toInt).toVector
    }

  val fileTarget = TreeSet(fileContent.toSeq: _*)
  implicit val rng = new Random(42)
  val model = Bilingual()

  val bc = BasinComputation(
    zone = Vector((0.0, 1.0), (0.0, 1.0), (0.0, 1.0)),
    depth = 15,
    pointInTarget = Vector(70.0 / 99, 24.0 / 99, 1.0 / 99),
    dynamic = model.dynamic,
    target = p => fileTarget.contains(p.map(c => math.round(c * 99).toInt)),
    controls = Vector((-0.1 to 0.1 by 0.01)),
    domain = (p: Vector[Double]) => p(0) + p(1) <= 1 && p.forall(_ >= 0))

  val (basin, step) = bc.approximate()

  //  val eroded = erode(bc, basin, rng)
  println(s"steps $step; volume ${volume(basin)}")
  //  println(volume(eroded))

  saveVTK3D(basin, s"/tmp/bilingual${bc.depth}CONTROL.vtk")
  //  saveVTK3D(eroded, s"/tmp/bilingual${bc.depth}_eroded.vtk")

}
