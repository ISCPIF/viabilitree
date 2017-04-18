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

//import scala.io.Source
//import scala.collection.immutable.TreeSet
//import Ordering.Implicits._
//import fr.iscpif.viability.basin._
//import fr.iscpif.kdtree.structure._
//import fr.iscpif.kdtree.export._
//import fr.iscpif.kdtree.algorithm.GridSampler
//import fr.iscpif.viability.control._
//import viabilitree.model.Control
//
//import scala.util.Random
//
//object BilingualBasin extends App with CaptureBasin with GridSampler {
//
//  def fileContent =
//    for {
//      l <- Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("targetData.txt")).getLines()
//    } yield {
//      l.split(" ").take(3).map(_.toInt).toSeq
//    }
//
//  lazy val fileTarget = TreeSet(fileContent.toSeq: _*)
//
//  //Closest point for sup norm
//  def target(p: Point): Boolean =
//    fileTarget.contains(p.map(c => math.round(c * 99).toInt))
//
//  def pointInTarget = Seq(70.0 / 99, 24.0 / 99, 1.0 / 99)
//
//  def zone = Seq(0.0 -> 1.0, 0.0 -> 1.0, 0.0 -> 1.0)
//  def domain = zone
//
//  def depth = 18
//
//  def dynamic(point: Point, control: Point) = Bilingual(point, control)
//
//  def controls = (-0.1 to 0.1 by 0.005).map(Control(_))
//
//  def dimension = 3
//
//  override def defined(p: Point) = p(0) + p(1) <= 1
//
//  implicit val rng = new Random(42)
//
//  for {
//    (s, i) <- apply.zipWithIndex
//  } {
//    println(i)
//    val output = s"/tmp/bilingual_$depth/basin$i.vtk"
//    saveVTK3D(s, output)
//  }
//
//}


import viabilitree.viability._
import viabilitree.viability.basin._
import viabilitree.export._

import io._
import scala.collection.immutable._
import scala.util.Random
import Ordering.Implicits._

object BilingualBasin extends App {

  def fileContent =
    Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("targetData.txt")).getLines().map { l =>
      l.split(" ").take(3).map(_.toInt).toVector
    }

  val fileTarget = TreeSet(fileContent.toSeq: _*)
  val rng = new Random(42)
  val model = Bilingual()

  val bc = BasinComputation(
    zone = Vector((0.0, 1.0), (0.0, 1.0), (0.0, 1.0)),
    depth = 15,
    pointInTarget =  Vector(70.0 / 99, 24.0 / 99, 1.0 / 99),
    dynamic = model.apply,
    target = p => fileTarget.contains(p.map(c => math.round(c * 99).toInt)),
    controls = Vector((-0.1 to 0.1 by 0.005)),
    defined = Some(p => p(0) + p(1) <= 1 && p.forall(_ >= 0))
  )

  val (basin, step) = approximate(bc, rng)
  println(volume(basin))
  println(step)

  saveVTK3D(basin, s"/tmp/bilingual${bc.depth}.vtk")
}
